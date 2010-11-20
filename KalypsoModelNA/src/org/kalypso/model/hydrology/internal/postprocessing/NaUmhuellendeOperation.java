/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.hydrology.internal.postprocessing;

import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.Date;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.jfree.data.time.DateRange;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.ogc.sensor.util.ZmlLink;

/**
 * Erzeugt umhüllende Kurven für KalypsoHydrology Ergebnisse.
 * 
 * @author Gernot Belger
 */
public class NaUmhuellendeOperation implements ICoreRunnableWithProgress
{
  private final NAOptimize m_naOptimize;

  private final File m_resultDir;

  private final NAControl m_metaControl;

  private URL m_zmlContext;

  public NaUmhuellendeOperation( final NAOptimize naOptimize, final NAControl metaControl, final File resultDir )
  {
    m_naOptimize = naOptimize;
    m_metaControl = metaControl;
    m_resultDir = resultDir;
    try
    {
      m_zmlContext = m_resultDir.toURI().toURL();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      m_zmlContext = null;
    }
  }

  /**
   * @see org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress#execute(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus execute( final IProgressMonitor monitor ) throws InvocationTargetException, CoreException
  {
    try
    {
      return doExecute( monitor );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();
    }
  }

  private IStatus doExecute( final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    final Date startForecast = m_metaControl.getStartForecast();
    final Date endForecast = m_metaControl.getSimulationEnd();
    final DateRange forecastRange = new DateRange( startForecast, endForecast );

    final ZmlLink resultLink = new ZmlLink( m_naOptimize.getResultLink(), m_zmlContext );
    final ZmlLink measuredLink = m_naOptimize.getPegelZRLink();

    final ZmlLink linkAblageMitte = new ZmlLink( m_naOptimize.getQAblageMittlererLink(), m_zmlContext );
    final ZmlLink linkAblageUnten = new ZmlLink( m_naOptimize.getQAblageUntererLink(), m_zmlContext );
    final ZmlLink linkAblageOben = new ZmlLink( m_naOptimize.getQAblageObererLink(), m_zmlContext );

    final Double accuracy = m_naOptimize.getPredictionAccuracy();
    final boolean useOffsetStart = m_naOptimize.doUseOffsetStartPred();
    final boolean useOffsetEnd = m_naOptimize.doUseOffsetEndPred();

    final UmhuellendeOperation umhuellendeOperation = new UmhuellendeOperation( resultLink, measuredLink, linkAblageMitte, linkAblageUnten, linkAblageOben, forecastRange );
    umhuellendeOperation.setAccuracy( accuracy );
    umhuellendeOperation.setUseOffsetStart( useOffsetStart );
    umhuellendeOperation.setUseOffsetEnd( useOffsetEnd );

    return umhuellendeOperation.execute( monitor );
  }
}
