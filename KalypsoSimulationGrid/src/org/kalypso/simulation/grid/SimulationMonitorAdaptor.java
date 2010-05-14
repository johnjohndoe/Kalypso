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
package org.kalypso.simulation.grid;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.simulation.core.ISimulationMonitor;

public class SimulationMonitorAdaptor implements IProgressMonitor {
	private final ISimulationMonitor m_monitor;

	private int m_worked;

	private int m_totalWork;

	public SimulationMonitorAdaptor(final ISimulationMonitor monitor) {
		m_monitor = monitor;
	}

	@Override
	public void beginTask(final String name, final int totalWork) {
		m_totalWork = totalWork;
		m_monitor.setMessage(name);
	}

	@Override
	public void done() {
		m_monitor.setProgress(100);
		m_monitor.setFinishInfo(IStatus.OK, "Finished.");
	}

	@Override
	public void internalWorked(final double work) {
	}

	@Override
	public boolean isCanceled() {
		return m_monitor.isCanceled();
	}

	@Override
	public void setCanceled(final boolean value) {
		if (value)
			m_monitor.cancel();
	}

	@Override
	public void setTaskName(final String name) {
		m_monitor.setMessage(name);
	}

	@Override
	public void subTask(final String name) {
		m_monitor.setMessage(name);
	}

	@Override
	public void worked(final int work) {
		m_monitor.setProgress((m_worked += work) / m_totalWork);
	}

}
