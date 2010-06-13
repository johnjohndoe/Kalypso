// Copyright (c) 1996-2002 Brian D. Carlstrom

package org.kalypso.kalypsomodel1d2d.sim;

import java.io.IOException;
import java.io.OutputStream;

/**
    MultiOutputStream

    Multiplex over multiple OutputStreams.

*/
public class MultiOutputStream extends OutputStream
{
    private int count;
    private OutputStreamEntry[] streams = new OutputStreamEntry[count];

    public MultiOutputStream ()
    {
    }

    public MultiOutputStream (OutputStream stream)
    {
        addStream(stream);
    }

    public void addStream (OutputStream s)
    {
        this.addStream(s, false);
    }

    public void addStream (OutputStream s, boolean buffered)
    {
        if (s == null) {
            return;
        }
        OutputStreamEntry[] newthis = new OutputStreamEntry[this.count + 1];
        for (int i = 0; i < this.count; i++) {
            if (this.streams[ i ].stream == s) {
                return;
            }
            newthis[ i ] = this.streams [ i ];
        }

        newthis[ this.count ] = new OutputStreamEntry(s, buffered);
        this.streams = newthis;
        this.count++;
    }

    public void removeStream (OutputStream s)
    {
        for (int i = 0; i < this.count; i++) {
            if (this.streams[ i ].stream == s) {
                this.streams[ i ] = this.streams [ this.count - 1 ];
                this.streams [ this.count - 1 ] = null;
                this.count--;
                break;
            }
        }
    }

    /**
        Called when we only want to flush cheap streams.

        For example, in logging, we always want to flush to the
        console, but we want to be lazy about forcing it to the file.
    */
    public void flushIfNotBuffered () throws IOException
    {
        for (int i = 0; i < this.count; i++) {
            if (!this.streams[i].buffered) {
                this.streams[i].stream.flush();
            }
        }
    }

    //--- OutputStream --------------------------------------------------------

    @Override
    public void write (int b) throws IOException
    {
        for (int i = 0; i < this.count; i++) {
            this.streams[i].stream.write(b);
        }
    }

    @Override
    public void write (byte[] b) throws IOException
    {
        for (int i = 0; i < this.count; i++) {
            this.streams[i].stream.write(b);
        }
    }

    @Override
    public void write (byte[] b, int offset, int length) throws IOException
    {
        for (int i = 0; i < this.count; i++) {
            this.streams[i].stream.write(b, offset, length);
        }
    }

    @Override
    public void flush () throws IOException
    {
        for (int i = 0; i < this.count; i++) {
            this.streams[i].stream.flush();
        }
    }

    public int count ()
    {
        return count;
    }
    
    public boolean isEmpty ()
    {
        return count == 0;
    }

    @Override
    public void close () throws IOException
    {
        for (int i = 0; i < this.count; i++) {
            this.streams[i].stream.close();
        }
    }
}

class OutputStreamEntry
{
    boolean buffered;
    OutputStream stream;

    public OutputStreamEntry (OutputStream s)
    {
        this.buffered = false;
        this.stream = s;
    }

    public OutputStreamEntry (OutputStream s, boolean buffered)
    {
        this.buffered = buffered;
        this.stream = s;
    }
}
