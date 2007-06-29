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
package org.kalypso.kalypsomodel1d2d.sim;

import java.util.List;

import org.kalypso.simulation.core.simspec.Modeldata;

/**
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 *
 */
public class CalculationUnitBasedModeldata extends Modeldata
{
  private String calUnitID;
  private Modeldata modelData;
  
  public CalculationUnitBasedModeldata(String calUnitID, Modeldata modelData )
  {
    this.calUnitID = calUnitID;
    this.modelData = modelData;
  }

  public String getCalUnitID( )
  {
    return calUnitID;
  }
  
  public void setCalUnitID( String calUnitID )
  {
    this.calUnitID = calUnitID;
  }
  
  /**
   * @param obj
   * @return
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( Object obj )
  {
    return modelData.equals( obj );
  }

  /**
   * @return
   * @see org.kalypso.simulation.core.simspec.Modeldata#getClearAfterCalc()
   */
  @Override
  public List<ClearAfterCalc> getClearAfterCalc( )
  {
    return modelData.getClearAfterCalc();
  }

  /**
   * @return
   * @see org.kalypso.simulation.core.simspec.Modeldata#getInput()
   */
  @Override
  public List<Input> getInput( )
  {
    return modelData.getInput();
  }

  /**
   * @return
   * @see org.kalypso.simulation.core.simspec.Modeldata#getOutput()
   */
  @Override
  public List<Output> getOutput( )
  {
    return modelData.getOutput();
  }

  /**
   * @return
   * @see org.kalypso.simulation.core.simspec.Modeldata#getTypeID()
   */
  @Override
  public String getTypeID( )
  {
    return modelData.getTypeID();
  }

  /**
   * @return
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return modelData.hashCode();
  }

  /**
   * @param value
   * @see org.kalypso.simulation.core.simspec.Modeldata#setTypeID(java.lang.String)
   */
  @Override
  public void setTypeID( String value )
  {
    modelData.setTypeID( value );
  }

  /**
   * @return
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return modelData.toString();
  }
  
  
}
