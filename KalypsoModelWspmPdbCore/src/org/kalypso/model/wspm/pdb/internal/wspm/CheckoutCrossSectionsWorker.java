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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.Set;

import javax.activation.MimeType;
import javax.activation.MimeTypeParseException;

import org.apache.commons.httpclient.URIException;
import org.apache.commons.httpclient.util.URIUtil;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.internal.WspmPdbCorePlugin;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.Image;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Gernot Belger
 */
public class CheckoutCrossSectionsWorker
{
  private final CheckoutDataMapping m_mapping;

  private final URL m_documentBase;

  public CheckoutCrossSectionsWorker( final CheckoutDataMapping mapping, final URL documentBase )
  {
    m_mapping = mapping;
    m_documentBase = documentBase;
  }

  public void execute( final IProgressMonitor monitor ) throws CoreException
  {
    final CrossSection[] crossSections = m_mapping.getCrossSections();

    monitor.beginTask( "Reading cross sections from database", crossSections.length );

    try
    {
      /* Convert the cross sections */
      for( final CrossSection crossSection : crossSections )
      {
        monitor.subTask( String.format( "Converting %s", crossSection.getStation() ) );
        insert( crossSection );
        ProgressUtilities.worked( monitor, 1 );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, WspmPdbCorePlugin.PLUGIN_ID, "Should never happen", e ); //$NON-NLS-1$
      throw new CoreException( status );
    }
    finally
    {
      ProgressUtilities.done( monitor );
    }
  }

  public void insert( final CrossSection section ) throws Exception
  {
    final WaterBody waterBody = section.getWaterBody();
    final WspmWaterBody wspmWaterBody = m_mapping.getWspmWaterBody( waterBody );

    final State state = section.getState();
    final TuhhReach reach = m_mapping.getReach( state );

    final IProfileFeature profile = insertProfile( section, wspmWaterBody );
    reach.createProfileSegment( profile, profile.getStation() );
  }

  private IProfileFeature insertProfile( final CrossSection section, final WspmWaterBody waterBody )
  {
    final IProfileFeature newProfile = waterBody.createNewProfile();
    newProfile.setProfileType( IWspmTuhhConstants.PROFIL_TYPE_PASCHE );

    final Geometry line = section.getLine();
    final String srs = line == null ? null : JTSAdapter.toSrs( line.getSRID() );
    newProfile.setSrsName( srs );

    final IProfil profile = newProfile.getProfil();

    final CrossSectionConverter converter = new CrossSectionConverter( section, profile );
    converter.execute();
    ProfileFeatureFactory.toFeature( profile, newProfile );

    convertDocuments( section, newProfile );

    return newProfile;
  }

  private void convertDocuments( final CrossSection section, final IProfileFeature profile )
  {
    final Set<Document> documents = section.getDocuments();
    for( final Document document : documents )
    {
      try
      {
        final String documentPath = document.getFilename();
        final String encodedPath = URIUtil.encodePath( documentPath );

        final URL documentURL = new URL( m_documentBase, encodedPath );
        final Image newImage = profile.addImage( documentURL );

        // TODO: convert other data as well
        // document.getCreationDate();
        // document.getCrossSection();
        // document.getMeasurementDate();
        // document.getEditingDate();
        // document.getEditingUser();

        final GM_Object location = convertGeometry( document.getLocation() );
        final String description = document.getDescription();
        final MimeType mimeType = convertMimeType( document.getMimetype() );
        final String name = document.getName();
        // document.getShotdirection();
        // document.getState();
        // document.getViewangle();
        // document.getWaterBody();

        newImage.setLocation( location );
        newImage.setName( name );
        newImage.setDescription( description );
        newImage.setMimeType( mimeType );
      }
      catch( final MalformedURLException e )
      {
        // TODO: error handling?!
        e.printStackTrace();
      }
      catch( final URIException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }
  }

  private MimeType convertMimeType( final String mimeType )
  {
    try
    {
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
