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
package org.kalypso.model.wspm.tuhh.ui.export.wspwin;

import org.kalypso.chart.ext.observation.TupleResultLineLayer;
import org.kalypso.commons.exception.CancelVisitorException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.manager.AbstractChartLayerVisitor;

/**
 * @author Dirk Kuch
 */
public class LengthSectionExportVisitor extends AbstractChartLayerVisitor
{
  private IObservation<TupleResult> m_observation;

  @Override
  public void visit( final IChartLayer layer ) throws CancelVisitorException
  {
    if( layer instanceof TupleResultLineLayer )
    {
      final IObservation<TupleResult> obs = ((TupleResultLineLayer) layer).getObservation();
      if( obs != null )
      {
        for( final IComponent comp : obs.getResult().getComponents() )
        {
          if( comp.getId().equals( IWspmConstants.LENGTH_SECTION_PROPERTY_STATION ) )
          {
            m_observation = obs;

            throw new CancelVisitorException();
          }
        }
      }
    }
  }

  public IObservation<TupleResult> getObservation( )
  {
    return m_observation;
  }
}