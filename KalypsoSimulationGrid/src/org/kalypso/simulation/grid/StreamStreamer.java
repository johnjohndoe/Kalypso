/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.simulation.grid;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.Semaphore;

import org.apache.commons.io.IOUtils;

public class StreamStreamer extends Thread {
	private final OutputStream m_os;

	private Semaphore m_crit = new Semaphore(1);
	private InputStream m_current = null;
	private InputStream m_pending = null;

	private boolean m_finished = false;

	private long m_totalRead = 0;

	public StreamStreamer(final OutputStream os) {
		m_os = os;
	}

	public void setInputStream(final InputStream is) {
		if (m_crit.tryAcquire()) {
			IOUtils.closeQuietly(m_current);
			m_current = is;
			m_crit.release();
		} else {
			m_pending = is;
		}
	}

	public void finish() {
		m_finished = true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Thread#run()
	 */
	@Override
	public void run() {
		while (true) {
			waitForNextStream();

			// check if finished and no stream is waiting
			if (m_finished && m_current == null && m_pending == null)
				break;

			try {
				// enter critical section
				m_crit.acquire();
				try {
					// read all available new bytes
					readFromCurrent();
					// flush output stream to get results immediately
					m_os.flush();
				} catch (final IOException ioe) {
					ioe.printStackTrace();
				}
			} catch (final InterruptedException e) {
				e.printStackTrace();
			} finally {
				IOUtils.closeQuietly(m_current);
				m_current = null;
				m_crit.release();
			}
			// not in critical section anymore
			setInputStream(m_pending);
			m_pending = null;
		}
	}

	private void readFromCurrent() throws IOException {
		final byte[] stuff = new byte[2048];
		// skip over bytes that have been read
		m_current.skip(m_totalRead);
		int read = m_current.read(stuff);
		while (read > 0) {
			m_totalRead += read;
			if (m_os != null) {
				m_os.write(stuff, 0, read);
			}
			read = m_current.read(stuff);
		}
	}

	private void waitForNextStream() {
		while (!m_finished && m_current == null) {
			try {
				Thread.sleep(1000);
			} catch (final InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
}