package libp2pwebrtcdirect

import (
	"encoding/json"
	"fmt"

	ic "github.com/libp2p/go-libp2p-core/crypto"
	peer "github.com/libp2p/go-libp2p-core/peer"
	multibase "github.com/multiformats/go-multibase"
	"github.com/pion/webrtc/v3"
)

type Signal struct {
	Type           string  `json:"type"`
	Sdp            string  `json:"sdp"`
	IdentityPubKey string  `json:"identity_pub_key"`
	TargetPeerId   peer.ID `json:"target_peer_id"`
	Signature      string  `json:"signature"`
}

func CreateSignal(config *connConfig, sdp *webrtc.SessionDescription, remoteID peer.ID) (Signal, error) {
	signal := Signal{
		Type:         sdp.Type.String(),
		Sdp:          sdp.SDP,
		TargetPeerId: remoteID,
	}

	identityPubKey, err := config.transport.LocalPubKeyAsProtobufBs58()
	if err != nil {
		return signal, err
	}
	signal.IdentityPubKey = identityPubKey

	input := signal.Type + signal.Sdp + signal.IdentityPubKey + signal.TargetPeerId.String()
	bytes, err := config.transport.localPrivKey.Sign([]byte(input))
	if err != nil {
		return signal, err
	}

	signature, err := multibase.Encode(multibase.Base58BTC, bytes)
	if err != nil {
		return signal, err
	}
	signal.Signature = signature

	return signal, nil
}

func (s *Signal) RemotePubKey() (ic.PubKey, error) {
	_, bytes, err := multibase.Decode(s.IdentityPubKey)
	if err != nil {
		return nil, err
	}
	return ic.UnmarshalPublicKey(bytes)
}

func encodeSignal(desc Signal) (string, error) {
	descData, err := json.Marshal(desc)
	if err != nil {
		return "", fmt.Errorf("failed to marshal description: %v", err)
	}

	descEnc, err := multibase.Encode(multibase.Base58BTC, descData)
	if err != nil {
		return "", fmt.Errorf("failed to encode description: %v", err)
	}
	return descEnc, nil
}

func decodeSignal(descEnc string) (Signal, error) {
	var desc Signal

	_, descData, err := multibase.Decode(descEnc)
	if err != nil {
		return desc, fmt.Errorf("failed to decode description: %v; descEnc: %s", err, descEnc)
	}

	err = json.Unmarshal(descData, &desc)
	if err != nil {
		return desc, fmt.Errorf("failed to unmarshal description: %v", err)
	}

	return desc, nil
}

func decodeSDP(descEnc string) (webrtc.SessionDescription, error) {
	var desc webrtc.SessionDescription

	_, descData, err := multibase.Decode(descEnc)
	if err != nil {
		return desc, fmt.Errorf("failed to decode description: %v; descEnc: %s", err, descEnc)
	}

	err = json.Unmarshal(descData, &desc)
	if err != nil {
		return desc, fmt.Errorf("failed to unmarshal description: %v", err)
	}

	return desc, nil
}
