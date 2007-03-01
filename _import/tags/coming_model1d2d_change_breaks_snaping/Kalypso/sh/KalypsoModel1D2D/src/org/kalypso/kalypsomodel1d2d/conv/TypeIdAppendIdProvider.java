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

/**
 * ID proides folowing this transformation scheme:
 * <ul>
 *      <li/>rma10 ids=<integer id>
 *      <li/>gml ids= <FE-element-type>+<integer id>
 * </ul>
 * 
 * @author Patrice Congo
 *
 */
public class TypeIdAppendIdProvider implements IModelElementIDProvider
{

  public TypeIdAppendIdProvider( )
  {
      
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider#gmlToRMA10S(org.kalypso.kalypsomodel1d2d.conv.ERma10sModelElementKey, java.lang.String)
   */
  public int gmlToRMA10S( 
                  ERma10sModelElementKey elemenKey, 
                  String gmlID ) 
                  throws IllegalArgumentException
  {
    return 0;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider#rma10sToGmlID(org.kalypso.kalypsomodel1d2d.conv.ERma10sModelElementKey, int)
   */
  public String rma10sToGmlID( 
                    ERma10sModelElementKey elementKey, 
                    int rma10sID ) 
                    throws IllegalArgumentException
  {
    return elementKey.toString()+rma10sID;
  }

//  private static final boolean hasRMA10SPrefix( char char0, char chr1 )
//  {
//    return true;
//  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider#allowExtenalID()
   */
  public boolean allowExtenalID( )
  {
    return false;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IModelElementIDProvider#inform(int, java.lang.String)
   */
  public void inform( int nativeID, String gmlID )
  {
    throw new RuntimeException(
          "This should not happen since this id provider does not"+
          "allow external ids");
  }
}
