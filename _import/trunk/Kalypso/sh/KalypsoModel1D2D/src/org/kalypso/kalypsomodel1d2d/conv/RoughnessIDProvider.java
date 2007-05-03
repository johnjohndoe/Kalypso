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

import java.util.ArrayList;

import org.kalypso.kalypsosimulationmodel.core.discr.IFEElement;

/**
 * @author Madanagopal
 *
 */
public class RoughnessIDProvider implements IRoughnessIDProvider
{

  private ArrayList<String> roughnessDBIDList;
  private ArrayList<Integer> nativeIDList;
  public static RoughnessIDProvider objectRoughness = null;
  private RoughnessIDProvider( )
  {
    roughnessDBIDList = new ArrayList<String>();
    nativeIDList = new ArrayList<Integer>();
  }
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider#getElementNativeRoughness(org.kalypso.kalypsosimulationmodel.core.discr.IFEElement)
   */
  
  public static synchronized RoughnessIDProvider getInstance() {    
    
    if (objectRoughness == null) {
      objectRoughness = new RoughnessIDProvider();
    }
    return objectRoughness;    
  }
  
  public int getElementNativeRoughness( IFEElement element ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub
    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider#getNativeID(java.lang.String)
   */
  public int getNativeID( String roughnessDBID ) throws IllegalArgumentException
  {
    if (roughnessDBIDList.contains( roughnessDBID )) 
    {
      return nativeIDList.get( roughnessDBIDList.indexOf( roughnessDBID ));
    }
    return UN_RESOLVED_NATIVE_ID;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider#getRoughnessDbId(int)
   */
  public String getRoughnessDbId( int nativeID )
  {
    if (nativeIDList.contains( nativeID ))
    {
     return roughnessDBIDList.get(nativeIDList.indexOf( nativeID ));
    }
     return null;
  }
  
  public void addToList(String roughnessDBID_, String nativeID_) {
    roughnessDBIDList.add( roughnessDBID_);
    nativeIDList.add( Integer.parseInt( nativeID_));    
  }

}
