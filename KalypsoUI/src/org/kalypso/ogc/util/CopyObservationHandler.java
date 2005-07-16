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
package org.kalypso.ogc.util;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Date;
import java.util.Properties;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.util.transformation.TransformationException;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Copies Observations describes in gml documents. Is used by the 'CopyObservationTask'.
 * 
 * @author belger
 */
public final class CopyObservationHandler
{
  public static void copyObserations( final IUrlResolver urlResolver, final URL gmlURL, final String featurePath,
      final String targetobservation, final URL context, final Source[] sources, final Properties metadata, final Date forecastFrom,
      final Date forecastTo, final PrintWriter logWriter ) throws Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( gmlURL );
    final FeatureType ft = workspace.getFeatureTypeFromPath( featurePath );
    if( ft == null )
      throw new TransformationException( "FeaturePath unbekannt: " + featurePath );

    final FeatureVisitor visitor = new CopyObservationFeatureVisitor( context, urlResolver, targetobservation, sources, metadata,
        forecastFrom, forecastTo, logWriter );
    workspace.accept( visitor, featurePath, FeatureVisitor.DEPTH_INFINITE );
  }

  public final static class Source
  {
    private final String property;

    private final Date from;

    private final Date to;

    private final String filter;

    public Source( final String prop, final Date dfrom, final Date dto ,final String filter)
    {
      this.property = prop;
      this.from = dfrom;
      this.to = dto;
      this.filter=filter;
    }

    public final Date getFrom()
    {
      return from;
    }

    public final String getProperty()
    {
      return property;
    }

    public final Date getTo()
    {
      return to;
    }

    public String getFilter()
    {
      // TODO Auto-generated method stub
      return filter;
    }
  }
}
