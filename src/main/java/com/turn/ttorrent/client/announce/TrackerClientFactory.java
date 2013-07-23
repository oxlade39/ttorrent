/**
 * Copyright (C) 2011-2012 Turn, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.turn.ttorrent.client.announce;

import com.turn.ttorrent.client.SharedTorrent;
import com.turn.ttorrent.common.Peer;

import java.net.URI;
import java.net.UnknownHostException;
import java.net.UnknownServiceException;

public class TrackerClientFactory {

    /**
     * Create a {@link TrackerClient} annoucing to the given tracker address.
     *
     *
     * @param torrent The torrent the tracker client will be announcing for.
     * @param peer The peer the tracker client will announce on behalf of.
     * @param tracker The tracker address as a {@link java.net.URI}.
     * @throws java.net.UnknownHostException If the tracker address is invalid.
     * @throws java.net.UnknownServiceException If the tracker protocol is not supported.
     */
    public TrackerClient createTrackerClient(SharedTorrent torrent, Peer peer,
                                             URI tracker) throws UnknownHostException, UnknownServiceException {
        String scheme = tracker.getScheme();

        if ("http".equals(scheme) || "https".equals(scheme)) {
            return new HTTPTrackerClient(torrent, peer, tracker);
        } else if ("udp".equals(scheme)) {
            return new UDPTrackerClient(torrent, peer, tracker);
        }

        throw new UnknownServiceException(
            "Unsupported announce scheme: " + scheme + "!");
    }
}
