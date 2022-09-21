package libp2pwebrtcdirect

import (
	"context"
	"crypto/ecdsa"
	"crypto/x509"
	"errors"
	"fmt"
	"io"
	"math"
	"net"
	"net/http"
	"runtime/debug"
	"time"

	logging "github.com/ipfs/go-log/v2"
	ic "github.com/libp2p/go-libp2p-core/crypto"
	peer "github.com/libp2p/go-libp2p-core/peer"
	tpt "github.com/libp2p/go-libp2p-core/transport"
	ma "github.com/multiformats/go-multiaddr"
	manet "github.com/multiformats/go-multiaddr/net"
	"github.com/pion/datachannel"
	"github.com/pion/webrtc/v3"
)

const dcBufSize = math.MaxUint16

type connConfig struct {
	transport *Transport
	maAddr    ma.Multiaddr
	addr      net.Addr
	isServer  bool
	remoteID  peer.ID
}

func newConnConfig(transport *Transport, maAddr ma.Multiaddr, isServer bool) (*connConfig, error) {
	httpMa := maAddr.Decapsulate(webrtcma)

	tcpMa := httpMa.Decapsulate(httpma)
	addr, err := manet.ToNetAddr(tcpMa)
	log.Warn("##webrtc::conn::newConnConfig>>", " maAddr: ", maAddr, " addr: ", addr)
	if err != nil {
		return nil, fmt.Errorf("failed to get net addr: %v", err)
	}

	// id, err := peer.Decode("QmaUEUoLWuDGBNY5FhGumAanKe79mEj5R2CHyAaqE5uCB3")
	// log.Warn("##webrtc::conn::newConnConfig>>", " id: ", id, " err: ", err)
	return &connConfig{
		transport: transport,
		maAddr:    maAddr,
		addr:      addr,
		isServer:  isServer,
		// remoteID:  id,
	}, nil
}

// Conn is a stream-multiplexing connection to a remote peer.
type Conn struct {
	config       *connConfig
	remoteID     peer.ID
	remotePubKey ic.PubKey

	peerConnection *webrtc.PeerConnection
	initChannel    datachannel.ReadWriteCloser

	buf      []byte
	bufStart int
	bufEnd   int
}

func newConn(config *connConfig, pc *webrtc.PeerConnection, initChannel datachannel.ReadWriteCloser) *Conn {
	conn := &Conn{
		config:         config,
		peerConnection: pc,
		initChannel:    initChannel,
		buf:            make([]byte, dcBufSize),
	}

	return conn
}

func dial(ctx context.Context, config *connConfig) (*Conn, error) {
	log := logging.Logger("codanet.libp2p")

	api := config.transport.api
	pc, err := api.NewPeerConnection(config.transport.webrtcOptions)
	if err != nil {
		return nil, err
	}

	dc, err := pc.CreateDataChannel("data", nil)
	if err != nil {
		return nil, err
	}

	offer, err := pc.CreateOffer(nil)
	if err != nil {
		return nil, err
	}
	log.Warn("##webrtc::dial>>", " offer: ", offer)

	// Complete ICE Gathering for single-shot signaling.
	gatherComplete := webrtc.GatheringCompletePromise(pc)
	err = pc.SetLocalDescription(offer)
	if err != nil {
		return nil, err
	}
	<-gatherComplete

	offerSignal, err := CreateSignal(config, pc.LocalDescription(), config.remoteID)
	if err != nil {
		return nil, err
	}

	offerEnc, err := encodeSignal(offerSignal)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequest("GET", "http://"+config.addr.String()+"/?signal="+offerEnc, nil)
	if err != nil {
		return nil, err
	}

	req = req.WithContext(ctx)

	answerEnc, err := func() ([]byte, error) {
		var client = &http.Client{}
		resp, err := client.Do(req)
		if err != nil {
			return nil, err
		}
		defer resp.Body.Close()

		answerEnc, err := io.ReadAll(resp.Body)
		if err != nil && err != io.EOF {
			return nil, err
		}
		return answerEnc, nil
	}()
	if err != nil {
		return nil, err
	}

	// TODO(zura): validate
	answerSignal, err := decodeSignal(string(answerEnc))
	if err != nil {
		return nil, err
	}
	remotePubKey, err := answerSignal.RemotePubKey()
	if err != nil {
		return nil, err
	}
	remoteID, err := peer.IDFromPublicKey(remotePubKey)
	if err != nil {
		return nil, err
	}

	answer, err := decodeSDP(string(answerEnc))
	if err != nil {
		return nil, err
	}
	log.Warn("##webrtc::dial>>", " answer: ", answer)

	if err := pc.SetRemoteDescription(answer); err != nil {
		return nil, err
	}

	var connectErr error = nil
	connected := make(chan *Conn)
	defer close(connected)
	dc.OnOpen(func() {
		detachedDc, err := dc.Detach()
		log.Warn("##webrtc::listen>>", " datachannel")
		if err != nil {
			log.Warn("##webrtc::listen>>", " datachannel detach error: ", err)
			connectErr = err
			connected <- nil
			return
		}
		c := newConn(config, pc, detachedDc)
		c.remoteID = remoteID
		c.remotePubKey = remotePubKey
		connected <- c
	})
	return <-connected, connectErr
}

// Close closes the stream muxer and the the underlying net.Conn.
func (c *Conn) Close() error {
	var err error
	if c.initChannel != nil {
		c.initChannel.Close()
		c.initChannel = nil
	}
	if c.peerConnection != nil {
		err = c.peerConnection.Close()
	}
	c.peerConnection = nil

	return err
}

// IsClosed returns whether a connection is fully closed, so it can
// be garbage collected.
func (c *Conn) IsClosed() bool {
	return c.peerConnection == nil
}

func (c *Conn) getPC() (*webrtc.PeerConnection, error) {
	if c.peerConnection == nil {
		return nil, errors.New("Conn closed")
	}

	return c.peerConnection, nil
}

func (c *Conn) checkInitChannel() datachannel.ReadWriteCloser {
	// Since a WebRTC offer can't be empty the offering side will have
	// an initial data channel opened. We return it here, the first time
	// OpenStream is called.
	if c.initChannel != nil {
		ch := c.initChannel
		c.initChannel = nil
		return ch
	}

	return nil
}

// LocalPeer returns our peer ID
func (c *Conn) LocalPeer() peer.ID {
	// TODO: Base on WebRTC security?
	return c.config.transport.localID
}

// LocalPrivateKey returns our private key
func (c *Conn) LocalPrivateKey() ic.PrivKey {
	// TODO: Base on WebRTC security?
	return nil

}

// RemotePeer returns the peer ID of the remote peer.
func (c *Conn) RemotePeer() peer.ID {
	if len(c.config.remoteID) > 0 {
		return c.config.remoteID
	}
	if len(c.remoteID) > 0 {
		return c.remoteID
	}

	dtls := c.peerConnection.DTLS()
	parsedRemoteCert, err := x509.ParseCertificate(dtls.GetRemoteCertificate())
	if err != nil {
		log.Error("Error when parsing peer DTLS certificate!", " Err: ", err, " cert: ", dtls.GetRemoteCertificate())
		debug.PrintStack()
		return ""
	}

	pub := parsedRemoteCert.PublicKey.(*ecdsa.PublicKey)
	pubkey := ic.NewECDSAPublicKey(pub)

	pkh, err := peer.IDFromPublicKey(&pubkey)
	if err != nil {
		log.Error("Error when trying to get `IDFromPublicKey`: ", err)
		return ""
	}

	c.remoteID = pkh
	log.Warn("##webrtc::conn::RemotePeer()>>", " err: ", err, " pubkeybasedID: ", pkh)
	return pkh
}

// RemotePublicKey returns the public key of the remote peer.
func (c *Conn) RemotePublicKey() ic.PubKey {
	// TODO: Base on WebRTC security?
	return c.remotePubKey
}

func (c *Conn) LocalAddr() net.Addr {
	return c.config.addr
}

func (c *Conn) RemoteAddr() net.Addr {
	// TODO(zura)
	return c.config.addr
}

// LocalMultiaddr returns the local Multiaddr associated
// with this connection
func (c *Conn) LocalMultiaddr() ma.Multiaddr {
	return c.config.maAddr
}

// RemoteMultiaddr returns the remote Multiaddr associated
// with this connection
func (c *Conn) RemoteMultiaddr() ma.Multiaddr {
	return c.config.maAddr
}

// Transport returns the transport to which this connection belongs.
func (c *Conn) Transport() tpt.Transport {
	return c.config.transport
}

func (w *Conn) Read(p []byte) (int, error) {
	var err error

	if w.bufEnd == 0 {
		n := 0
		n, err = w.initChannel.Read(w.buf)
		w.bufEnd = n
	}

	n := 0
	if w.bufEnd-w.bufStart > 0 {
		n = copy(p, w.buf[w.bufStart:w.bufEnd])
		w.bufStart += n

		if w.bufStart >= w.bufEnd {
			w.bufStart = 0
			w.bufEnd = 0
		}
	}

	log.Warn("DC.Read: ", p[:n], " str: ", string(p[:n]))

	return n, err
}

func (w *Conn) Write(p []byte) (n int, err error) {
	log.Warn("DC.Write: ", p, " str: ", string(p))
	if len(p) > dcBufSize {
		return w.initChannel.Write(p[:dcBufSize])
	}
	return w.initChannel.Write(p)
}

func (w *Conn) SetDeadline(t time.Time) error {
	return nil
}

func (w *Conn) SetReadDeadline(t time.Time) error {
	return nil
}

func (w *Conn) SetWriteDeadline(t time.Time) error {
	return nil
}
