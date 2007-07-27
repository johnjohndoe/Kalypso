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

import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DTo2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DToCLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;

/**
 * Convert calculation unit Junction context to RMA10S native file entries
 * 
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
@SuppressWarnings( { "unchecked", "hiding" })
public class JunctionContextConverter
{
  final IFEDiscretisationModel1d2d m_discModel1d2d;

  final Formatter m_formatter;

  final ICalculationUnit1D2D m_calcUnit;

  final INativeIDProvider m_nativeIDProvider;

  public JunctionContextConverter( final IFEDiscretisationModel1d2d discModel1d2d, final ICalculationUnit1D2D calUnit, final INativeIDProvider nativeIDProvider, final Formatter formatter2dFile )
  {
    this.m_discModel1d2d = discModel1d2d;
    this.m_calcUnit = calUnit;
    this.m_nativeIDProvider = nativeIDProvider;
    this.m_formatter = formatter2dFile;
  }

  /**
   * Writes the unit junction context to the 2d file
   */
  public void write( )
  {
    // TODO: the junction elements must be filtered by the calc-unit (org got from the calc-unit)
    final List<IFE1D2DComplexElement> complexElements = m_discModel1d2d.getComplexElements();
    for( final IFE1D2DComplexElement complexElement : complexElements )
    {
      // if (TypeInfo.isJuntionContext( complexElement ))
      if( complexElement instanceof IJunctionContext1DTo2D )
      {
        final IJunctionContext1DTo2D ele1D2D = (IJunctionContext1DTo2D) complexElement;
        // if (CalUnitOps.isJunctionContextOf( calUnit, ele1D2D ))
        // {
        // ILineElement continuityLine = ele1D2D.getContinuityLine();
        // IElement1D element1D = ele1D2D.getElement1D();
        // IPolyElement element2D = ele1D2D.getElement2D();
        // IFE1D2DNode target1DNode = ele1D2D.getTarget1DNode();
        // formatter2dFile.format( "CCL%10d%10.1d%n", nativeIDProvider.getID(continuityLine));
        // }
      }
      if( complexElement instanceof IJunctionContext1DToCLine )
      {
        final IJunctionContext1DToCLine ele1DToCLine = (IJunctionContext1DToCLine) complexElement;
        if( CalcUnitOps.isJunctionContextOf( m_calcUnit, ele1DToCLine ) )
        {
          final ILineElement continuityLine = ele1DToCLine.getContinuityLine();
          final IElement1D element1D = ele1DToCLine.getElement1D();
          final IFE1D2DNode target1DNode = ele1DToCLine.getTarget1DNode();

          final int transitionElementID = m_nativeIDProvider.getBoundaryLineID( ele1DToCLine );
          final int element1DID = m_nativeIDProvider.getBoundaryLineID( element1D );
          final int boundaryLineID = m_nativeIDProvider.getBoundaryLineID( continuityLine );
          final int node1DID = m_nativeIDProvider.getBoundaryLineID( target1DNode );

          m_formatter.format( "TL%10d%10d%10d%10d%n", transitionElementID, element1DID, boundaryLineID, node1DID );
        }
      }
    }
  }

  public static final void write( final IFEDiscretisationModel1d2d discModel1d2d, final ICalculationUnit calUnit, final INativeIDProvider nativeIDProvider, final Formatter formatter2dFile )
  {
    if( calUnit instanceof ICalculationUnit1D2D )
    {
      final JunctionContextConverter converter = new JunctionContextConverter( discModel1d2d, (ICalculationUnit1D2D) calUnit, nativeIDProvider, formatter2dFile );
      converter.write();
    }
  }
}
