package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"sync"
	"time"

	capnp "capnproto.org/go/capnp/v3"
	"github.com/go-errors/errors"
	net "github.com/libp2p/go-libp2p-core/network"
	peer "github.com/libp2p/go-libp2p-core/peer"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	mdns "github.com/libp2p/go-libp2p/p2p/discovery"
	"github.com/multiformats/go-multiaddr"
	"github.com/prometheus/client_golang/prometheus"
	ipc "libp2p_ipc"
)

func newApp() *app {
	return &app{
		P2p:                      nil,
		Ctx:                      context.Background(),
		Subs:                     make(map[uint64]subscription),
		Topics:                   make(map[string]*pubsub.Topic),
		ValidatorMutex:           &sync.Mutex{},
		Validators:               make(map[uint64]*validationStatus),
		Streams:                  make(map[uint64]net.Stream),
		OutChan:                  make(chan *capnp.Message, 64),
		Out:                      bufio.NewWriter(os.Stdout),
		AddedPeers:               []peer.AddrInfo{},
		MetricsRefreshTime:       time.Minute,
		metricsCollectionStarted: false,
		metricsServer:            nil,
	}
}

func (app *app) NextId() uint64 {
	app.counterMutex.Lock()
	defer app.counterMutex.Unlock()
	app.counter = app.counter + 1
	return app.counter
}

func parseMultiaddrWithID(ma multiaddr.Multiaddr, id peer.ID) (*codaPeerInfo, error) {
	ipComponent, tcpMaddr := multiaddr.SplitFirst(ma)
	if !(ipComponent.Protocol().Code == multiaddr.P_IP4 || ipComponent.Protocol().Code == multiaddr.P_IP6) {
		return nil, badRPC(errors.New(fmt.Sprintf("only IP connections are supported right now, how did this peer connect?: %s", ma.String())))
	}

	tcpComponent, _ := multiaddr.SplitFirst(tcpMaddr)
	if tcpComponent.Protocol().Code != multiaddr.P_TCP {
		return nil, badRPC(errors.New("only TCP connections are supported right now, how did this peer connect?"))
	}

	port, err := strconv.Atoi(tcpComponent.Value())
	if err != nil {
		return nil, err
	}

	return &codaPeerInfo{Libp2pPort: uint16(port), Host: ipComponent.Value(), PeerID: id}, nil
}

func addrInfoOfString(maddr string) (*peer.AddrInfo, error) {
	multiaddr, err := multiaddr.NewMultiaddr(maddr)
	if err != nil {
		return nil, err
	}
	info, err := peer.AddrInfoFromP2pAddr(multiaddr)
	if err != nil {
		return nil, err
	}

	return info, nil
}

func (app *app) writeMsg(msg *capnp.Message) {
	if app.NoUpcalls {
		return
	}

	app.OutChan <- msg
}

func (app *app) updateConnectionMetrics() {
	info := app.P2p.ConnectionManager.GetInfo()
	connectionCountMetric.Set(float64(info.ConnCount))
}

// TODO: {peer,protocol}-{min,max,avg}
func (app *app) checkBandwidth() {
	totalIn := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_total_bandwidth_in",
		Help: "The total incoming bandwidth used",
	})
	totalOut := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_total_bandwidth_out",
		Help: "The total outgoing bandwidth used",
	})
	rateIn := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_bandwidth_rate_in",
		Help: "The incoming bandwidth rate",
	})
	rateOut := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_bandwidth_rate_out",
		Help: "The outging bandwidth rate",
	})

	var err error

	err = prometheus.Register(totalIn)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register total_bandwidth_in; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(totalOut)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register total_bandwidth_out; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(rateIn)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register bandwidth_rate_in; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(rateOut)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register bandwidth_rate_out; perhaps we've already done so", err.Error())
		return
	}

	for {
		stats := app.P2p.BandwidthCounter.GetBandwidthTotals()
		totalIn.Set(float64(stats.TotalIn))
		totalOut.Set(float64(stats.TotalOut))
		rateIn.Set(stats.RateIn)
		rateOut.Set(stats.RateOut)

		time.Sleep(app.MetricsRefreshTime)
	}
}

func (app *app) checkPeerCount() {
	peerCount := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_peer_count",
		Help: "The total number of peers in our network",
	})
	connectedPeerCount := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_connected_peer_count",
		Help: "The total number of peers we are actively connected to",
	})

	var err error

	err = prometheus.Register(peerCount)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register peer_count; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(connectedPeerCount)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register connected_peer_count; perhaps we've already done so", err.Error())
		return
	}

	for {
		peerCount.Set(float64(len(app.P2p.Host.Network().Peers())))
		connectedPeerCount.Set(float64(app.P2p.ConnectionManager.GetInfo().ConnCount))

		time.Sleep(app.MetricsRefreshTime)
	}
}

func (app *app) checkMessageStats() {
	msgMax := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_message_max_stats",
		Help: "The max size of network message received",
	})
	msgAvg := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_message_avg_stats",
		Help: "The average size of network message received",
	})
	msgMin := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_message_min_stats",
		Help: "The min size of network message received",
	})

	err := prometheus.Register(msgMax)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register message_max_stats; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(msgAvg)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register message_avg_stats; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(msgMin)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register message_min_stats; perhaps we've already done so", err.Error())
		return
	}

	for {
		msgStats := app.P2p.MsgStats.GetStats()
		msgMin.Set(msgStats.Min)
		msgAvg.Set(msgStats.Avg)
		msgMax.Set(msgStats.Max)

		time.Sleep(app.MetricsRefreshTime)
	}
}

func (app *app) checkLatency() {
	latencyMin := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_latency_min",
		Help: fmt.Sprintf("The minimum latency (recorded over %s)", latencyMeasurementTime),
	})
	latencyMax := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_latency_max",
		Help: fmt.Sprintf("The maximum latency (recorded over %s)", latencyMeasurementTime),
	})
	latencyAvg := prometheus.NewGauge(prometheus.GaugeOpts{
		Name: "Mina_libp2p_latency_avg",
		Help: fmt.Sprintf("The average latency (recorded over %s)", latencyMeasurementTime),
	})

	var err error

	err = prometheus.Register(latencyMin)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register latency_min; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(latencyMax)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register latency_max; perhaps we've already done so", err.Error())
		return
	}

	err = prometheus.Register(latencyAvg)
	if err != nil {
		app.P2p.Logger.Debugf("couldn't register latency_avg; perhaps we've already done so", err.Error())
		return
	}

	for {
		peers := app.P2p.Host.Peerstore().Peers()
		if len(peers) > 0 {
			sum := 0.0
			minimum := math.MaxFloat64
			maximum := 0.0

			for _, peer := range peers {
				app.P2p.Host.Peerstore().RecordLatency(peer, latencyMeasurementTime)
				latency := float64(app.P2p.Host.Peerstore().LatencyEWMA(peer))

				sum += latency
				minimum = math.Min(minimum, latency)
				maximum = math.Max(maximum, latency)
			}

			latencyMin.Set(minimum)
			latencyMax.Set(maximum)
			latencyAvg.Set(sum / float64(len(peers)))
		} else {
			latencyMin.Set(0.0)
			latencyMax.Set(0.0)
			latencyAvg.Set(0.0)
		}

		time.Sleep(app.MetricsRefreshTime)
	}
}

func setPeerInfoList(m ipc.PeerInfo_List, peerInfos []codaPeerInfo) {
	// TODO check that it works
	for i := 0; i < len(peerInfos); i++ {
		setPeerInfo(m.At(i), &peerInfos[i])
	}
}

func setMultiaddrList(m ipc.Multiaddr_List, addrs []multiaddr.Multiaddr) {
	// TODO check that it works
	for i := 0; i < len(addrs); i++ {
		panicOnErr(m.At(i).SetRepresentation(addrs[i].String()))
	}
}

func handleStreamReads(app *app, stream net.Stream, idx uint64) {
	go func() {
		buf := make([]byte, 4096)
		tot := uint64(0)
		for {
			n, err := stream.Read(buf)

			if n != 0 {
				app.writeMsg(mkStreamMessageReceivedUpcall(idx, buf[:n]))
			}

			if err != nil && err != io.EOF {
				app.writeMsg(mkStreamLostUpcall(idx, fmt.Sprintf("read failure: %s", err.Error())))
				return
			}

			tot += uint64(n)

			if err == io.EOF {
				app.P2p.MsgStats.UpdateMetrics(tot)
				break
			}
		}
		app.writeMsg(mkStreamCompleteUpcall(idx))
		err := stream.Close()
		if err != nil {
			app.P2p.Logger.Warning("Error while closing the stream: ", err.Error())
		}
	}()
}

func beginMDNS(app *app, foundPeerCh chan peerDiscovery) error {
	mdns, err := mdns.NewMdnsService(app.Ctx, app.P2p.Host, time.Minute, "_coda-discovery._udp.local")
	if err != nil {
		return err
	}
	app.P2p.Mdns = &mdns
	l := &mdnsListener{
		FoundPeer: foundPeerCh,
		app:       app,
	}
	mdns.RegisterNotifee(l)

	return nil
}

func findPeerInfo(app *app, id peer.ID) (*codaPeerInfo, error) {
	if app.P2p == nil {
		return nil, needsConfigure()
	}

	conns := app.P2p.Host.Network().ConnsToPeer(id)

	if len(conns) == 0 {
		if app.UnsafeNoTrustIP {
			app.P2p.Logger.Info("UnsafeNoTrustIP: pretending it's localhost")
			return &codaPeerInfo{Libp2pPort: 0, Host: "127.0.0.1", PeerID: id}, nil
		}
		return nil, badp2p(errors.New("tried to find peer info but no open connections to that peer ID"))
	}

	conn := conns[0]

	maybePeer, err := parseMultiaddrWithID(conn.RemoteMultiaddr(), conn.RemotePeer())
	if err != nil {
		return nil, err
	}
	return maybePeer, nil
}
