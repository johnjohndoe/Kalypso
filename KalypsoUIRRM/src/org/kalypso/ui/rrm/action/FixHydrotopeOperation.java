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
package org.kalypso.ui.rrm.action;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.binding.IHydrotope;
import org.kalypso.model.hydrology.binding.NAHydrotop;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * Fixes hydrotopes: sets the catchment link to every hydrotope.
 * 
 * @author Gernot Belger
 */
public class FixHydrotopeOperation implements ICoreRunnableWithProgress
{
  private final NAHydrotop m_hydrotopes;

  private final NaModell m_naModell;

  public FixHydrotopeOperation( final NAHydrotop hydrotopes, final NaModell naModell )
  {
    m_hydrotopes = hydrotopes;
    m_naModell = naModell;
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws CoreException, InterruptedException
  {
    final IFeatureBindingCollection<IHydrotope> hydrotopes = m_hydrotopes.getHydrotopes();
    final IFeatureBindingCollection<Catchment> catchments = m_naModell.getCatchments();

    monitor.beginTask( "Intersecting catchments with hydrotopes", catchments.size() );

    int count = 0;
    for( final Catchment catchment : catchments )
    {
// if( (count % 10) == 0 )
      monitor.subTask( String.format( "%d/%d", count++, catchments.size() ) ); //$NON-NLS-1$

      Thread.sleep( 100 );

      ProgressUtilities.worked( monitor, 1 );
    }

    return new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), "B‰h!" );
  }

}
