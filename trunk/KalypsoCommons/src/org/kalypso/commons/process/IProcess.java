/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.commons.process;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.contribs.java.lang.ICancelable;

/**
 * This interface represents an abstract process. <br>
 * It is still something similar to {@link Process}, but allows process implementations that behave differently (such as
 * running on a different machine, or similar).
 * 
 * @author Gernot Belger
 */
public interface IProcess
{
  /**
   * The environment used to start the new process.<br>
   * Initially, the map is initialised to the target environment of the current process and/or target machine (depending
   * on the implementation).<br>
   * The map can be modified (before calling
   * {@link #startProcess(ICancelable, long, OutputStream, OutputStream, InputStream, int)}.
   */
  public Map<String, String> environment( );

  /**
   * Sets a timeout for this process. If set to a value <code>&gt; 1</code>, the process will automatically killed after
   * the given time in milliseconds and a {@link ProcessTimeoutException} will be thrown.
   */
  public void setTimeout( long timeout );

  /**
   * Executes the process and waits for it to end.
   * 
   *@param stdOut
   *          Standard output of the process will be written into this stream.
   *@param stdErr
   *          Standard error output of the process will be written into this stream.
   *@param stdIn
   *          Contents of this stream will be piped into the standard input of the process.
   *@param cancelable
   *          The process will be killed if {@link ICancelable#isCanceled()} returns <code>true</code>.
   * @return The process'es exit code.
   *@throws ProcessTimeoutException
   *           If a timeout was set (see {@link #setTimeout(long)}) and the process run longer than this specified time.
   */
  public int startProcess( final OutputStream stdOut, final OutputStream stdErr, final InputStream stdIn, final ICancelable cancelable ) throws IOException, ProcessTimeoutException, OperationCanceledException;

  /**
   * Sets a progress monitor (optional). Process implementations may ignore the monitor.
   * 
   * @param monitor
   *          the progress monitor to use for reporting progress to the user. It is the caller's responsibility to call
   *          done() on the given monitor. Accepts null, indicating that no progress should be reported and that the
   *          operation cannot be cancelled.
   */
  public void setProgressMonitor( final IProgressMonitor monitor );
}
