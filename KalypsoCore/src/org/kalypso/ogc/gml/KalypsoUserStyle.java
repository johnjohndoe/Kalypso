/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml;

import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.model.feature.event.ModellEventProviderAdapter;
import org.kalypsodeegree.xml.Marshallable;

/**
 * Wrapped UserStyle to provide fireModellEvent Method
 * 
 * @author bce
 */
public class KalypsoUserStyle extends ModellEventProviderAdapter implements UserStyle, Marshallable
{
  private boolean m_disposed = false;

  protected UserStyle m_userStyle;

  protected final String m_styleName;

  public KalypsoUserStyle( final UserStyle style, final String styleName )
  {
    m_userStyle = style;
    m_styleName = styleName;
  }

  /**
   * @see org.kalypsodeegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {
    return ( (Marshallable)m_userStyle ).exportAsXML();
  }

  public void addFeatureTypeStyle( FeatureTypeStyle featureTypeStyle )
  {
    m_userStyle.addFeatureTypeStyle( featureTypeStyle );
  }

  public String getAbstract()
  {
    return m_userStyle.getAbstract();
  }

  public FeatureTypeStyle[] getFeatureTypeStyles()
  {
    return m_userStyle.getFeatureTypeStyles();
  }

  public String getName()
  {
    return m_userStyle.getName();
  }

  public String getTitle()
  {
    return m_userStyle.getTitle();
  }

  public boolean isDefault()
  {
    return m_userStyle.isDefault();
  }

  public void removeFeatureTypeStyle( FeatureTypeStyle featureTypeStyle )
  {
    m_userStyle.removeFeatureTypeStyle( featureTypeStyle );
  }

  public void setAbstract( String abstract_ )
  {
    m_userStyle.setAbstract( abstract_ );
  }

  public void setDefault( boolean default_ )
  {
    m_userStyle.setDefault( default_ );
  }

  public void setFeatureTypeStyles( FeatureTypeStyle[] featureTypeStyles )
  {
    m_userStyle.setFeatureTypeStyles( featureTypeStyles );
  }

  public void setName( String name )
  {
    m_userStyle.setName( name );
  }

  public void setTitle( String title )
  {
    m_userStyle.setTitle( title );
  }

  public boolean isDisposed()
  {
    return m_disposed;
  }

  public void dispose()
  {
    m_disposed = true;
  }
}
