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
package org.kalypso.ui.gazetter.view;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.LabelProvider;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author FlowsAd
 */
public class GazetteerLabelProvider extends LabelProvider
{
  private final QName m_labelProperty;

  public GazetteerLabelProvider( QName labelProperty )
  {
    super();
    m_labelProperty = labelProperty;
  }

  /**
   * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
   */
  @Override
  public String getText( Object element )
  {
    if( element == GazetteerConstants.NO_SELECTION_IN_COMBO )
      return "bitte waehlen";
    if( element instanceof Feature )
    {
      final Feature feature = (Feature) element;
      final Object text = feature.getProperty( m_labelProperty );
      if( text != null )
        return text.toString();
      return "fehler";
    }
    return super.getText( element );
  }

}
