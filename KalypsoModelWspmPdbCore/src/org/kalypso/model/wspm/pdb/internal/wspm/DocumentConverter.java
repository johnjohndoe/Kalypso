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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.net.URI;
import java.util.Comparator;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.activation.MimeType;
import javax.activation.MimeTypeParseException;

import org.apache.commons.lang3.StringUtils;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.Image;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Point;

/**
 * @author Holger Albert
 */
public class DocumentConverter
{
  private final URI m_documentBase;

  public DocumentConverter( final URI documentBase )
  {
    m_documentBase = documentBase;
  }

  public void convertDocuments( final WaterBody waterBody, final WspmWaterBody wspmWaterBody )
  {
    if( m_documentBase == null )
      return;

    final SortedSet<Document> sortedDocuments = sortDocuments( waterBody.getDocuments() );
    for( final Document document : sortedDocuments )
    {
      final String documentPath = document.getFilename();
      final URI documentURL = org.eclipse.core.runtime.URIUtil.append( m_documentBase, documentPath );
      final Image newImage = wspmWaterBody.addImage( documentURL );

      setDataToImage( document, newImage );
    }
  }

  public void convertDocuments( final CrossSection crossSection, final IProfileFeature profile )
  {
    if( m_documentBase == null )
      return;

    final SortedSet<Document> sortedDocuments = sortDocuments( crossSection.getDocuments() );
    for( final Document document : sortedDocuments )
    {
      final String documentPath = document.getFilename();
      final URI documentURL = org.eclipse.core.runtime.URIUtil.append( m_documentBase, documentPath );
      final Image newImage = profile.addImage( documentURL );

      setDataToImage( document, newImage );
    }
  }

  public void convertDocuments( final State state, final TuhhReach reach )
  {
    if( m_documentBase == null )
      return;

    final SortedSet<Document> sortedDocuments = sortDocuments( state.getDocuments() );
    for( final Document document : sortedDocuments )
    {
      final String documentPath = document.getFilename();
      final URI documentURL = org.eclipse.core.runtime.URIUtil.append( m_documentBase, documentPath );
      final Image newImage = reach.addImage( documentURL );

      setDataToImage( document, newImage );
    }
  }

  private SortedSet<Document> sortDocuments( final Set<Document> documents )
  {
    // Sort documents, so pictures come first.
    // Avoids, that the user looks at a warning when looking at a cross section first time
    final Comparator<Document> documentComparator = new DocumentPictureComparator();
    final SortedSet<Document> sortedDocuments = new TreeSet<>( documentComparator );
    sortedDocuments.addAll( documents );

    return sortedDocuments;
  }

  private void setDataToImage( final Document document, final Image image )
  {
    // TODO: convert other data as well
    // document.getCreationDate();
    // document.getCrossSection();
    // document.getMeasurementDate();
    // document.getEditingDate();
    // document.getEditingUser();
    // document.getShotdirection();
    // document.getState();
    // document.getViewangle();
    // document.getWaterBody();

    final GM_Object location = convertGeometry( document.getLocation() );
    final String name = document.getName();
    final String description = document.getDescription();
    final MimeType mimeType = convertMimeType( document.getMimetype() );

    image.setLocation( location );
    image.setName( name );
    image.setDescription( description );
    image.setMimeType( mimeType );
  }

  private MimeType convertMimeType( final String mimeType )
  {
    try
    {
      if( StringUtils.isBlank( mimeType ) )
        return null;

      return new MimeType( mimeType );
    }
    catch( final MimeTypeParseException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private GM_Object convertGeometry( final Point location )
  {
    try
    {
      if( location == null )
        return null;

      final String srs = JTSAdapter.toSrs( location.getSRID() );
      return JTSAdapter.wrap( location, srs );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }
}