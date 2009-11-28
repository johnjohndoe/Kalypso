/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.ui.view;

import java.util.HashMap;

import de.openali.odysseus.chart.framework.model.style.IStyle;
import de.openali.odysseus.chart.framework.util.StyleUtils;

/**
 * @author kimwerner
 */
public class AbstractLayerStyleProvider implements ILayerStyleProvider
{
  private HashMap<String, IStyle> m_styles = null;

  protected void createStyles( )
  {
    // TODO: read styles from *.kod file or add Styles here
  }

  final protected void addStyle( final String id, final IStyle style )
  {
    if( m_styles == null )
    {
      m_styles = new HashMap<String, IStyle>();
    }
    m_styles.put( id, style );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.ILayerStyleProvider#getStyleFor(java.lang.String, java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  public <T extends IStyle> T getStyleFor( String id, Class<T> defaultStyle )
  {
    if( m_styles == null )
    {
      m_styles = new HashMap<String, IStyle>();
      createStyles();
    }
    final IStyle style = m_styles.get( id );
    if( (style != null) )
      return (T) style;
    final IStyle newStyle = defaultStyle == null ? null : StyleUtils.getDefaultStyle( defaultStyle );
    if( newStyle != null )
      m_styles.put( id, newStyle );
    return (T) newStyle;

  }
}
