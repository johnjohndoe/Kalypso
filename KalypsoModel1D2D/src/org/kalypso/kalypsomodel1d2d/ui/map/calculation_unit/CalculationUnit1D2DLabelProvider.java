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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;


import org.eclipse.jface.viewers.LabelProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;


/**
 * @author Madanagopal
 *
 */
public class CalculationUnit1D2DLabelProvider extends LabelProvider
{
  public CalculationUnit1D2DLabelProvider()
  {   
  }

  @Override
  public String getText( Object element )
  {
    if( element instanceof ICalculationUnit1D || element instanceof ICalculationUnit2D)
    {

      String name = ((ICalculationUnit) element).getName();
      if( name != null )
      {
        return name;
      }
      else
      {
        return ((ICalculationUnit) element).getId();
      }
    }
    else
    {
      throw new RuntimeException( "Only ICalculation1D / ICalculation2D are supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }
}
