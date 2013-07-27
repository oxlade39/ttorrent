package com.turn.ttorrent.common;

import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.List;

/**
 * @author dan
 */
public interface ITorrent {
    String getName();

    String getComment();

    String getCreatedBy();

    long getSize();

    List<String> getFilenames();

    boolean isMultifile();

    byte[] getInfoHash();

    String getHexInfoHash();

    List<List<URI>> getAnnounceList();

    int getTrackerCount();

    boolean isSeeder();

}
