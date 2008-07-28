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
package org.kalypso.ant;

import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.math.BigDecimal;
import java.net.URL;
import java.util.List;
import java.util.logging.Level;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.util.logging.ILogger;
import org.kalypso.contribs.java.util.logging.LoggerUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Ein Task, welcher anhand von Thiessen Polygonen und Einzugsgebieten die Gewichtungsfaktoren ermittelt.
 * 
 * @author Gernot Belger
 */
public class ThiessenWeightTask extends Task
{
  public static final BigDecimal BIG_ZERO = new BigDecimal( "0.0" );

  /** Path to the gml: should be of type: {org.kalypso.dwd.rcm}RainfallCatchmentModel */
  private URL m_gmlURL;

  /** Path to features containing the thiessen polygones */
  private String m_thiessenFeaturePath;

  /** Path to property within the thiessen features which contains the polygone geometry */
  private String m_thiessenPropertyPath;

  /** Path to features containing the areas which shall be intersected with the thiessen polygones */
  private String m_areaFeaturePath;

  /** Path to property withing the area features which contains the polygone geometry */
  private String m_areaPropertyPath;

  /** Path to property withing the area features which shall contain the weight features */
  private String m_weightPropertyPath;

  private int m_fractionDigits;

  private String m_ombrometerReferencePropertyPath;

  public void setAreaFeaturePath( String areaFeaturePath )
  {
    m_areaFeaturePath = areaFeaturePath;
  }

  public void setAreaPropertyPath( String areaPropertyPath )
  {
    m_areaPropertyPath = areaPropertyPath;
  }

  public void setGmlURL( URL gmlURL )
  {
    m_gmlURL = gmlURL;
  }

  public void setThiessenFeaturePath( String thiessenFeaturePath )
  {
    m_thiessenFeaturePath = thiessenFeaturePath;
  }

  public void setThiessenPropertyPath( String thiessenPropertyPath )
  {
    m_thiessenPropertyPath = thiessenPropertyPath;
  }

  public void setWeightPropertyPath( String weightPropertyPath )
  {
    m_weightPropertyPath = weightPropertyPath;
  }

  public void setFractionDigits( final int fractionDigits )
  {
    m_fractionDigits = fractionDigits;
  }

  public void setOmbrometerReferencePropertyPath( String ombrometerReferencePropertyPath )
  {
    m_ombrometerReferencePropertyPath = ombrometerReferencePropertyPath;
  }
  
  @Override
  public void execute() throws BuildException
  {
    try
    {
      final Project antProject = getProject();
      ILogger logger = new ILogger()
      {
        public void log( Level level, int msgCode, String message )
        {
          final String outString = LoggerUtilities.formatLogStylish( level, msgCode, message );
          if( antProject == null )
            System.out.println( outString );
          else
            antProject.log( outString );
        }
      };

      logger.log( Level.INFO, LoggerUtilities.CODE_NONE, "Lade Gebietsniederschlagsmodell..." );

      // load catchment model
      if( m_gmlURL == null )
        throw new Exception( "Eingangs-GML nicht gesetzt" );

      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( m_gmlURL, null );
      if( workspace == null )
        throw new Exception( "Modell konnte nicht geladen werden" );

      final String targetCRS = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      final IFeatureType rainfallWeightFT = workspace.getFeatureType( "RainfallWeight" );

      if( m_areaFeaturePath == null )
        throw new Exception( "Pfad auf Einzugsgebietsflächen nicht gesetzt" );

      final Object areaFeatureFromPath = workspace.getFeatureFromPath( m_areaFeaturePath );
      final Feature[] areaFeatures = FeatureHelper.getFeaturess( areaFeatureFromPath );

      if( m_thiessenFeaturePath == null )
        throw new BuildException( "Pfad auf Thiessen-Features nicht gesetzt" );

      final Object thiessenFeatureFromPath = workspace.getFeatureFromPath( m_thiessenFeaturePath );
      final Feature[] thiessenFeatures = FeatureHelper.getFeaturess( thiessenFeatureFromPath );

      logger.log( Level.INFO, LoggerUtilities.CODE_NONE, "Zurodnungsfaktoren werden ermittelt..." );

      for( int i = 0; i < areaFeatures.length; i++ )
      {
        final Feature areaFeature = areaFeatures[i];
        calculateWeights( areaFeature, thiessenFeatures, rainfallWeightFT, logger );
      }

      BufferedWriter writer = null;
      try
      {
        final IUrlResolver resolver = UrlResolverSingleton.getDefault();

        final OutputStreamWriter osw = resolver.createWriter( m_gmlURL );
        writer = new BufferedWriter( osw );

        GmlSerializer.serializeWorkspace( writer, workspace, osw.getEncoding() );
 
        writer.close();
      }
      finally
      {
        IOUtils.closeQuietly( writer );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new BuildException( e.getLocalizedMessage(), e );
    }
  }

  private void calculateWeights( final Feature areaFeature, final Feature[] thiessenFeatures,
      final IFeatureType rainfallWeightFT, final ILogger logger ) throws GM_Exception
  {
    logger.log( Level.INFO, LoggerUtilities.CODE_NONE, "Berechne Faktoren für: " + areaFeature.getProperty( "name" ) );

    final IPropertyType areaPT = areaFeature.getFeatureType().getProperty( m_areaPropertyPath );
    if( areaPT == null )
      throw new BuildException( "Keine Property mit gefunden mit Namen: " + m_areaPropertyPath );

    final GM_Object areaGeom = (GM_Object)areaFeature.getProperty( m_areaPropertyPath );
    final Polygon areaPolygon = areaGeom == null ? null : (Polygon)JTSAdapter.export( areaGeom );

    for( int i = 0; i < thiessenFeatures.length; i++ )
    {
      final Feature thiessenFeature = thiessenFeatures[i];
      final IPropertyType thiessenPT = thiessenFeature.getFeatureType().getProperty( m_thiessenPropertyPath );
      if( thiessenPT == null )
        throw new BuildException( "Keine Property mit gefunden mit Namen: " + m_thiessenPropertyPath );

      final GM_Object thiessenGeom = (GM_Object)thiessenFeature.getProperty( m_thiessenPropertyPath );
      final Polygon thiessenPolygon = thiessenGeom == null ? null : (Polygon)JTSAdapter.export( thiessenGeom );

      final BigDecimal factor = calcWeight( areaPolygon, thiessenPolygon );

      logger.log( Level.FINE, LoggerUtilities.CODE_NONE, "\tOmbrometer '" + thiessenFeature.getProperty( "name" )
          + "' Faktor: " + factor );

      if( factor.compareTo( BIG_ZERO ) > 0 )
        createWeight( areaFeature, thiessenFeature, rainfallWeightFT, factor );
    }
  }

  private void createWeight( final Feature areaFeature, final Feature thiessenFeature,
      final IFeatureType rainfallWeightFT, final BigDecimal factor )
  {
    final FeatureList weights = (FeatureList)areaFeature.getProperty( m_weightPropertyPath );

    final Feature weightFeature = FeatureFactory.createFeature( weights.getParentFeature(), weights.getParentFeatureTypeProperty(), "weight", rainfallWeightFT, false );

    final IPropertyType weightPT = areaFeature.getFeatureType().getProperty( m_weightPropertyPath );
    if( weightPT == null )
      throw new BuildException( "Keine Property mit gefunden mit Namen: " + m_weightPropertyPath );


    weights.add( weightFeature );

    weightFeature.setProperty( "weight", factor );
    weightFeature.setProperty( m_ombrometerReferencePropertyPath, thiessenFeature.getId() );
  }

  private BigDecimal calcWeight( final Polygon areaPolygon, final Polygon thiessenPolygon )
  {
    if( areaPolygon == null )
      throw new BuildException( "Es gibt ein Einzugsgebiet ohne Geometrie!" );

    if( thiessenPolygon == null )
      return BIG_ZERO;

    final Geometry geometry = areaPolygon.intersection( thiessenPolygon );
    if( geometry == null )
      return BIG_ZERO;

    final double totalArea = areaPolygon.getArea();
    final double subArea = geometry.getArea();

    final double weight = subArea / totalArea;
    
    return new BigDecimal( weight ).setScale( m_fractionDigits, BigDecimal.ROUND_HALF_UP );
  }
}
