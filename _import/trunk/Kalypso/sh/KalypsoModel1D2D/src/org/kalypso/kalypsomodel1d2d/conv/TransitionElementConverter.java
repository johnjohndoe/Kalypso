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
package org.kalypso.kalypsomodel1d2d.conv;

import java.util.Formatter;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IContinuityLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ITransitionElement;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;

/**
 * Convert calculation unit Junction context to RMA10S native file entries
 * 
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
public class TransitionElementConverter
{
  private final IFEDiscretisationModel1d2d m_discModel1d2d;

  private final Formatter m_formatter;

  private final ICalculationUnit1D2D m_calcUnit;

  private final INativeIDProvider m_nativeIDProvider;

  public TransitionElementConverter( final IFEDiscretisationModel1d2d discModel1d2d, final ICalculationUnit1D2D calcUnit, final INativeIDProvider nativeIDProvider, final Formatter formatter2dFile )
  {
    m_discModel1d2d = discModel1d2d;
    m_calcUnit = calcUnit;
    m_nativeIDProvider = nativeIDProvider;
    m_formatter = formatter2dFile;
  }

  /**
   * Writes the unit junction context to the 2d file
   */
  public void write( ) throws SimulationException
  {
    System.out.println("come");
    final List<IFE1D2DComplexElement> complexElements = m_discModel1d2d.getComplexElements();
    for( final IFE1D2DComplexElement complexElement : complexElements )
    {
      if( complexElement instanceof ITransitionElement )
      {
        final ITransitionElement transitionElement = (ITransitionElement) complexElement;
        final List<IFELine> transitionElementContinuityLines = transitionElement.getContinuityLines();
        final List calcUnitContinuityLines = m_calcUnit.getContinuityLines();
        if( calcUnitContinuityLines.containsAll( transitionElementContinuityLines ) )
        {
          final int transitionElementID = m_nativeIDProvider.getConversionID( transitionElement );
          final IContinuityLine1D line1D;
          final int node1D_ID;
          int element1D_ID = -1;
          final int line2D_ID;
          if( transitionElementContinuityLines.get( 0 ) instanceof IContinuityLine1D )
          {
            line1D = (IContinuityLine1D) transitionElementContinuityLines.get( 0 );
            line2D_ID = m_nativeIDProvider.getConversionID( transitionElementContinuityLines.get( 1 ) );
          }
          else
          {
            line2D_ID = m_nativeIDProvider.getConversionID( transitionElementContinuityLines.get( 0 ) );
            line1D = (IContinuityLine1D) transitionElementContinuityLines.get( 1 );
          }
          final IFE1D2DNode node1D = line1D.getNodes().get( 0 );
          node1D_ID = m_nativeIDProvider.getConversionID( node1D );
          final IFeatureWrapperCollection<IFeatureWrapper2> containers = node1D.getContainers();
          for( final IFeatureWrapper2 container : containers )
          {
            if( container instanceof IFE1D2DEdge )
              if( m_calcUnit.getElements().contains( container ) )
                element1D_ID = m_nativeIDProvider.getConversionID( container );
          }
          if(element1D_ID == -1)
            throw new SimulationException("Transition line cannot be exported: cannot find 1D element.", null);
          m_formatter.format( "TL%10d%10d%10d%10d%n", transitionElementID, element1D_ID, line2D_ID, node1D_ID ); //$NON-NLS-1$
        }
      }
    }
  }
}
