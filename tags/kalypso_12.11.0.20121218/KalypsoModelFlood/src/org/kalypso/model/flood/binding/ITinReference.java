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
 *  Lesser General License for more details.
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
package org.kalypso.model.flood.binding;

import java.math.BigDecimal;
import java.net.URL;
import java.util.Date;

import javax.xml.namespace.QName;

import org.kalypso.model.flood.schema.UrlCatalogModelFlood;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public interface ITinReference extends Feature
{
  enum SOURCETYPE
  {
    gml( "GML" ), //$NON-NLS-1$
    hmo( "HMO-Datei" ), //$NON-NLS-1$
    shape( "ESRI Shape-Datei" ); //$NON-NLS-1$

    private final String m_label;

    private SOURCETYPE( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  QName QNAME = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "TinReference" ); //$NON-NLS-1$

  QName QNAME_PROP_MIN = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "min" ); //$NON-NLS-1$

  QName QNAME_PROP_MAX = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "max" ); //$NON-NLS-1$

  QName QNAME_PROP_SOURCE_LOCATION = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "sourceLocation" ); //$NON-NLS-1$

  QName QNAME_PROP_SOURCE_PATH = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "sourcePath" ); //$NON-NLS-1$

  QName QNAME_PROP_SOURCE_DATE = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "sourceDate" ); //$NON-NLS-1$

  QName QNAME_PROP_SOURCE_TYPE = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "sourceType" ); //$NON-NLS-1$

  QName QNAME_PROP_TIN = new QName( UrlCatalogModelFlood.NS_MODEL_FLOOD, "tin" ); //$NON-NLS-1$

  void setMin( BigDecimal min );

  BigDecimal getMin( );

  void setMax( BigDecimal max );

  BigDecimal getMax( );

  void setTin( GM_TriangulatedSurface surface );

  GM_TriangulatedSurface getTin( );

  void setSourceLocation( URL location );

  URL getSourceLocation( );

  SOURCETYPE getSourceType( );

  void setSourceFeaturePath( final GMLXPath path );

  GMLXPath getSourceFeaturePath( );

  void setUpdateDate( Date date );

  void setSourceType( final SOURCETYPE type );

  Date getUpdateDate( );

  // Non-Binding helpers

  /**
   * Finds the (linearly-)interpolated value of this tin at the given position.<br>
   * If the position is not covered by this surface, {@link Double#NaN} will be returned.
   *
   * @param pos
   *            A position in the same coordinate system as this geometry.
   */
  double getValue( final GM_Position pos );

  /**
   * Returns the parent of this referenz as a {@link IRunoffEvent}.
   *
   * @return The paretn feature adapted to {@link IRunoffEvent}, ma be <code>null</code>.
   */
  IRunoffEvent getRunoffEvent( );

  GM_Triangle getTriangle( final GM_Position pos );
}
