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
import java.net.URL;

import org.apache.commons.io.FileUtils;
import org.kalypso.contribs.java.xml.XMLHelper;
import org.kalypso.convert.namodel.optimize.CalibrationConfig;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.binding.NAControl;
import org.kalypso.model.hydrology.binding.NAModellControl;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.optimize.transform.OptimizeModelUtils;
import org.kalypso.optimize.transform.ParameterOptimizeContext;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.w3c.dom.Document;

/**
 * @author Gernot Belger
 */
public class NaSimulationData
{
  private final GMLWorkspace m_parameterWorkspace;

  private final GMLWorkspace m_sudsWorkspace;

  private final GMLWorkspace m_hydrotopWorkspace;

  private final GMLWorkspace m_synthNWorkspace;

  private final GMLWorkspace m_modelWorkspace;

  private final GMLWorkspace m_lzsimWorkspace;

  private final NAModellControl m_naModellControl;

  private final NAControl m_metaControl;

  public NaSimulationData( final URL modelUrl, final URL controlURL, final URL metaUrl, final URL parameterUrl, final URL hydrotopUrl, final URL sudsUrl, final URL syntNUrl, final URL lzsimUrl ) throws Exception
  {
    final GMLWorkspace controlWorkspace = GmlSerializer.createGMLWorkspace( controlURL, null );
    m_naModellControl = (NAModellControl) controlWorkspace.getRootFeature();

    m_modelWorkspace = loadModelWorkspace( modelUrl );
    final GMLWorkspace metaWorkspace = GmlSerializer.createGMLWorkspace( metaUrl, null );
    m_metaControl = (NAControl) metaWorkspace.getRootFeature();

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

    transformModelToHydrotopeCrs();
  }

  private void transformModelToHydrotopeCrs( )
  {
    final String targetCS = determineHydrotopeCrs();
    if( targetCS == null )
      return;

    final TransformVisitor visitor = new TransformVisitor( targetCS );
    final Feature rootFeature = m_modelWorkspace.getRootFeature();
    m_modelWorkspace.accept( visitor, rootFeature, FeatureVisitor.DEPTH_INFINITE ); //$NON-NLS-1$
  }

  private String determineHydrotopeCrs( )
  {
    // TODO: this is wrong: why transform the model to the hydrotope workspace?
    // Normally, we should transform both (better every loaded model) into the current kalypso crs
    final IFeatureType hydrotopeFT = m_hydrotopWorkspace.getGMLSchema().getFeatureType( NaModelConstants.HYDRO_ELEMENT_FT );
    // FIXME: access hydrotopes directly, do not fetch them into a big list!
    final Feature[] hydroFES = m_hydrotopWorkspace.getFeatures( hydrotopeFT );
    String targetCS = null;
    for( int i = 0; i < hydroFES.length && targetCS == null; i++ )
    {
      // FIXME: performance: is it really necessary to check ALL geometries here???
      final GM_Object geom = (GM_Object) hydroFES[i].getProperty( NaModelConstants.HYDRO_PROP_GEOM );
      if( geom != null && geom.getCoordinateSystem() != null )
        targetCS = geom.getCoordinateSystem();
    }
    return targetCS;
  }

  private GMLWorkspace loadModelWorkspace( final URL modelUrl ) throws SimulationException
  {
    // Apply optimization parameters
    // TODO: does not really belong here, shouldn't the optimize job do this?
    final CalibrationConfig config = new CalibrationConfig( m_naModellControl );
    final ParameterOptimizeContext[] calContexts = config.getCalContexts();

    try
    {
      final Document modelDoc = XMLHelper.getAsDOM( modelUrl, true );

      OptimizeModelUtils.initializeModel( modelDoc, calContexts );

      return GmlSerializer.createGMLWorkspace( modelDoc, modelUrl, null );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
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

  public NAModellControl getNaControl( )
  {
    return m_naModellControl;
  }

  public GMLWorkspace getModelWorkspace( )
  {
    return m_modelWorkspace;
  }

  public NAControl getMetaControl( )
  {
    return m_metaControl;
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
