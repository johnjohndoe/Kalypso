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
package org.kalypso.kalypsomodel1d2d.ui.viewer;

import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;

/**
 * Label provider for {@link org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls}
 * This label provides can have two states:
 * <ul>
 *      <li/>{@link #ROUGHNESS_ONLY} to provide label for tables 
 *          showing only roughnesses
 *      <li/>{@link #ROUGHNESS_WITH_CORRECTION} to provides labels 
 *      for tables showing corrected roughnesses *          
 * </ul>
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class RoughnessClsLabelProvider 
                extends LabelProvider 
                implements ITableLabelProvider
{
  /**
   * State key for providing label to tables showing only roughnesses
   */
  public static final int ROUGHNESS_ONLY=0;
  
  /**
   * State key for providing label to tables showing only 
   * corrected roughnesses
   * 
   */
  public static final int ROUGHNESS_WITH_CORRECTION=1;
  
  /** roughness name column number for roughness only table*/
  public static final int NON_COR_ROUGHNESS_NAME_COL=0;
  
  /** ks column number for roughness only table*/
  public static final int NON_COR_KS_COL=1;
  
  /** axay column number for roughness only table*/
  public static final int NON_COR_AXAY_COL=2; 
  
  /** dp column number for roughness only table*/
  public static final int NON_COR_DP_COL=3;
  
  
  /** roughness name column number for corrected roughness table*/
  public static final int COR_ROUGHNESS_NAME_COL=0;
  
  /** ks column number for corrected roughness table*/
  public static final int COR_KS_COL=1;
  
  /** corrected ks column number for corrected roughness table*/
  public static final int COR_KS_COL_COR=1;
  
  /** ax ay column number for corrected roughness table*/
  public static final int COR_AXAY=2;
  
  /** corrected ax ay column number for corrected roughness table*/
  public static final int COR_AXAY_COR=2;
  
  /** dp column number for corrected roughness table*/
  public static final int COR_DP=3;
  
  /** corrected dp column number for corrected roughness table*/
  public static final int COR_DP_COR=3;
  
  
  
  private int configState;
  
  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( Object element )
  {
    if(element instanceof IRoughnessCls)
    {
      return ((IRoughnessCls)element).getName();
    }
    else
    {
      return super.getText(element);
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  @Override
  public Image getColumnImage( Object element, int columnIndex )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  @Override
  public String getColumnText( Object element, int columnIndex )
  {
    if(configState==ROUGHNESS_ONLY)
    {
      return getColumnTextForRoughnessOnly( element, columnIndex );
    }
    else if(configState==ROUGHNESS_WITH_CORRECTION)
    {
      return null;
    }
    else
    {
      return null;
    }
    
  }
  
  /**
   * Sets the config state for this label provide.
   * legal values are:
   * <ul>
   *    <li/>{@link #ROUGHNESS_ONLY}
   *    <li/>{@link #ROUGHNESS_WITH_CORRECTION}
   * </ul>
   */
  public void setConfigState( 
                  int configState )
                  throws IllegalArgumentException      
  {
    if(!( configState!=ROUGHNESS_ONLY || 
          configState!=ROUGHNESS_WITH_CORRECTION))
    {
      throw new IllegalArgumentException(
          "configState value is illegal:"+configState); //$NON-NLS-1$
    }
    this.configState = configState;
  }
  
 
  
  private final String getColumnTextForRoughnessOnly( Object element, int columnIndex )
  {
    if(element instanceof IRoughnessCls)
    {
//      return ((IRoughnessCls)element).getName();
      switch(columnIndex)
      {
        case NON_COR_ROUGHNESS_NAME_COL:
        {
          return ((IRoughnessCls)element).getName();
        }
        case NON_COR_KS_COL:
        {
          return String.valueOf(((IRoughnessCls)element).getKs());
        }
        case NON_COR_AXAY_COL:
        {
          return String.valueOf(((IRoughnessCls)element).getAxAy());
        }
        case NON_COR_DP_COL:
        {
          return String.valueOf(((IRoughnessCls)element).getDp());
        }
        default:
        {
          return toUnknownColSpecStr( element, columnIndex );
        }
      }
    }
    else
    {
      return toUnknownColSpecStr( element, columnIndex );//super.getText(element);
    }
  }
  
  
  private static final String toUnknownColSpecStr(Object element, int columnIndex)
  {
//    StringBuffer buf=new StringBuffer(64);
//    buf.append( "Unknown Colum Spec element=" );
//    buf.append( element );
//    buf.append( " colunm" );
//    buf.append( columnIndex );
//    return buf.toString();
    return "unknown"; //$NON-NLS-1$
  }
}
