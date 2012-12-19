/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.pdb.wspm;

import java.net.URI;

import javax.activation.MimeType;

import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.URIUtil;
import org.hibernate.Session;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.Image;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

/**
 * @author Holger Albert
 */
public class CheckinDocumentWorker
{
  private final URI m_documentBase;

  public CheckinDocumentWorker( final URI documentBase )
  {
    m_documentBase = documentBase;
  }

  public void createDocuments( final Session session, final State state, final TuhhReach reach )
  {
    if( state == null || reach == null )
      return;

    final IFeatureBindingCollection<Image> images = reach.getImages();
    for( final Image image : images )
    {
      final MimeType mimeType = image.getMimeType();
      final URI uri = image.getUri();

      final Document document = new Document();
      document.setCreationDate( state.getCreationDate() );
      document.setDescription( image.getDescription() );
      document.setEditingDate( state.getEditingDate() );
      document.setEditingUser( state.getEditingUser() );
      document.setFilename( asFilename( uri ) );
      document.setLocation( asPoint( image.getLocation() ) );
      document.setMeasurementDate( state.getMeasurementDate() ); // bad, should come from image
      document.setMimetype( mimeType == null ? null : mimeType.toString() );
      document.setName( asName( uri ) );
      // document.setShotdirection( null );
      // document.setViewangle( null );

      // REMARK: we set cross section + water body to null here: this is a state document!
      // I.e. if the state is removed, also this document will be destroyed which is ok.
      document.setCrossSection( null );
      document.setState( state );
      document.setWaterBody( null );

      if( session != null )
        session.save( document );
    }
  }

  public void createDocuments( final Session session, final CrossSection section, final IProfileFeature profile )
  {
    if( section == null || profile == null )
      return;

    final IFeatureBindingCollection<Image> images = profile.getImages();
    for( final Image image : images )
    {
      final MimeType mimeType = image.getMimeType();
      final URI uri = image.getUri();

      final Document document = new Document();
      document.setCreationDate( section.getCreationDate() );
      document.setDescription( image.getDescription() );
      document.setEditingDate( section.getEditingDate() );
      document.setEditingUser( section.getEditingUser() );
      document.setFilename( asFilename( uri ) );
      document.setLocation( asPoint( image.getLocation() ) );
      document.setMeasurementDate( section.getMeasurementDate() ); // bad, should come from image
      document.setMimetype( mimeType == null ? null : mimeType.toString() );
      document.setName( asName( uri ) );
      // document.setShotdirection( null );
      // document.setViewangle( null );

      // REMARK: we set state + water body to null here: this is a profile document!
      // I.e. if the profile is removed, also this document will be destroyed which is ok.
      document.setCrossSection( section );
      document.setState( null );
      document.setWaterBody( null );

      if( session != null )
        session.save( document );
    }
  }

  private String asName( final URI uri )
  {
    if( uri == null )
      return "-"; //$NON-NLS-1$

    final String unencoded = URIUtil.toUnencodedString( uri );
    return FilenameUtils.getName( unencoded );
  }

  /**
   * Makes the absolute local uri relative to the document server
   */
  private String asFilename( final URI uri )
  {
    if( uri == null )
      return null;

    final String fullLocation = uri.toString();
    if( m_documentBase == null )
      return fullLocation;

    final URI relative = URIUtil.makeRelative( uri, m_documentBase );
    return URIUtil.toUnencodedString( relative );
  }

  private com.vividsolutions.jts.geom.Point asPoint( final GM_Object location )
  {
    if( location == null )
      return null;

    try
    {
      final GM_Point centroid = location.getCentroid();
      return (com.vividsolutions.jts.geom.Point)JTSAdapter.export( centroid );
    }
    catch( final GM_Exception e )
    {
      // will never happen
      e.printStackTrace();
      return null;
    }
  }
}