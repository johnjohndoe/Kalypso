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
package org.kalypso.convert.namodel;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.URL;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.optimize.CalibrationConfig;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.w3c.dom.Document;

/**
 * @author Gernot Belger
 */
public class NaSimulationData
{
  private final GMLWorkspace m_controlWorkspace;

  private final GMLWorkspace m_metaWorkspace;

  private final GMLWorkspace m_parameterWorkspace;

  private final GMLWorkspace m_sudsWorkspace;

  private final GMLWorkspace m_hydrotopWorkspace;

  private final GMLWorkspace m_synthNWorkspace;

  private final GMLWorkspace m_modelWorkspace;

  private final GMLWorkspace m_lzsimWorkspace;

  public NaSimulationData( final URL modelUrl, final File newModellFile, final URL controlURL, final URL metaUrl, final URL parameterUrl, final URL hydrotopUrl, final URL sudsUrl, final URL syntNUrl, final URL lzsimUrl ) throws Exception
  {
    m_controlWorkspace = GmlSerializer.createGMLWorkspace( controlURL, null );
    m_modelWorkspace = loadModelWorkspace( modelUrl, newModellFile );
    m_metaWorkspace = GmlSerializer.createGMLWorkspace( metaUrl, null );
    m_parameterWorkspace = GmlSerializer.createGMLWorkspace( parameterUrl, null );
    // FIXME: do not load hydrotopes, if preprocessed files exist
    m_hydrotopWorkspace = GmlSerializer.createGMLWorkspace( hydrotopUrl, null );

// m_landuseWorkspace = loadAndCheckForFile( landuseUrl );
    m_sudsWorkspace = loadAndCheckForFile( sudsUrl );
    m_lzsimWorkspace = loadAndCheckForFile( lzsimUrl );

    final String syntNpath = syntNUrl == null ? null : syntNUrl.getFile();
    final File syntNGML = syntNpath == null ? null : new File( syntNpath, "calcSynthN.gml" ); //$NON-NLS-1$
    if( syntNGML != null && syntNGML.exists() )
      m_synthNWorkspace = GmlSerializer.createGMLWorkspace( syntNGML, null );
    else
      m_synthNWorkspace = null;
  }

  private GMLWorkspace loadModelWorkspace( final URL modelUrl, final File newModellFile ) throws SimulationException
  {
    // Apply optimization parameters
    final CalibrationConfig config = new CalibrationConfig();
    config.addFromNAControl( m_controlWorkspace.getRootFeature() );

    try
    {
      final Document modelDoc = XMLHelper.getAsDOM( modelUrl, true );

      OptimizeModelUtils.initializeModel( modelDoc, config.getCalContexts() );

      // FIXME: performance: we do not need to write the model again: directly SAX-parse the model from DOM!

      // TODO: take charset from Document
      final String charset = "UTF-8"; //$NON-NLS-1$
      final Writer writer = new OutputStreamWriter( new FileOutputStream( newModellFile ), charset );
      final Transformer t = TransformerFactory.newInstance().newTransformer();
      t.transform( new DOMSource( modelDoc ), new StreamResult( writer ) );
      writer.close();

      final GMLWorkspace modellWorkspace = GmlSerializer.createGMLWorkspace( newModellFile.toURI().toURL(), null );
      ((GMLWorkspace_Impl) modellWorkspace).setContext( modelUrl );

      return modellWorkspace;
    }
    catch( final Exception e )
    {
      throw new SimulationException( "Failed to initialize model workspace", e );
    }
  }

  private static GMLWorkspace loadAndCheckForFile( final URL location ) throws Exception
  {
    if( location == null )
      return null;

    final File sudsFile = FileUtils.toFile( location );
    if( sudsFile == null )
      return GmlSerializer.createGMLWorkspace( location, null );

    /* Silently ignore if file does not exists */
    if( !sudsFile.exists() )
      return null;

    return GmlSerializer.createGMLWorkspace( location, null );
  }

  public GMLWorkspace getControlWorkspace( )
  {
    return m_controlWorkspace;
  }

  public GMLWorkspace getModelWorkspace( )
  {
    return m_modelWorkspace;
  }

  public GMLWorkspace getMetaWorkspace( )
  {
    return m_metaWorkspace;
  }

  public GMLWorkspace getParameterWorkspace( )
  {
    return m_parameterWorkspace;
  }

  public GMLWorkspace getHydrotopWorkspace( )
  {
    return m_hydrotopWorkspace;
  }

  public GMLWorkspace getSudsWorkspace( )
  {
    return m_sudsWorkspace;
  }

  public GMLWorkspace getSynthNWorkspace( )
  {
    return m_synthNWorkspace;
  }

  public GMLWorkspace getLzsimWorkspace( )
  {
    return m_lzsimWorkspace;
  }

}
