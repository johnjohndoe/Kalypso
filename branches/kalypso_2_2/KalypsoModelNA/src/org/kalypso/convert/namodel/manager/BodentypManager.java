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

package org.kalypso.convert.namodel.manager;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.schema.binding.suds.IAbstractSwale;
import org.kalypso.convert.namodel.schema.binding.suds.ISwale;
import org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.simulation.core.SimulationException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author huebsch
 */
public class BodentypManager extends AbstractManager
{
  private final NAConfiguration m_conf;

  private final IFeatureType m_bodentypFT;

  private final IFeatureType m_bodenartFT;

  private final Map<String, Map<String, Double[]>> m_soilTypes = new LinkedHashMap<String, Map<String, Double[]>>();

  // private static final String BodArtParameterPropName = "soilLayerParameterMember";

  public BodentypManager( final GMLSchema parameterSchema, final NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_conf = conf;
    m_bodentypFT = parameterSchema.getFeatureType( NaModelConstants.PARA_Soiltype_FT );
    m_bodenartFT = parameterSchema.getFeatureType( NaModelConstants.PARA_SoilLayerParameter_FT );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.IFeatureType)
   */
  @Override
  public String mapID( final int id, final IFeatureType ft )
  {
    // TODO check if maximal allowed ASCII variable length constraint is fulfilled
    return ft.getQName().getLocalPart() + id;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( final URL url ) throws Exception
  {
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    // Kommentarzeilen
    // TODO What is the point of this printout??
    for( int i = 0; i <= 2; i++ )
    {
      final String line = reader.readLine();
      if( line == null )
        return null;

      System.out.println( String.format( "%6d : %s", reader.getLineNumber(), line ) ); //$NON-NLS-1$
    }
    while( (fe = readNextFeature( reader )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( final LineNumberReader reader ) throws Exception
  {
    final HashMap<String, String> propCollector = new HashMap<String, String>();
    String line;
    // 1
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$
    createProperties( propCollector, line, 1 );

    // generate id:
    String asciiStringId = propCollector.get( "name" ); //$NON-NLS-1$
    final Feature feature = getFeature( asciiStringId, m_bodentypFT );

    int ianz = Integer.parseInt( propCollector.get( "ianz" ) ); //$NON-NLS-1$
    HashMap<String, String> bodArtPropCollector = new HashMap<String, String>();
    // BodArtParameterMember
    for( int i = 0; i < ianz; i++ )
    {
      Feature bodArtParameterFeature = createFeature( m_bodenartFT );
      line = reader.readLine();
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.BodentypManager.4", i, line ) ); //$NON-NLS-1$ 
      createProperties( bodArtPropCollector, line, 2 );
      // BodArtLink
      // final FeatureProperty BodArtNameProp = (FeatureProperty)bodArtPropCollector.get( "name" );
      String asciiBodArtId = bodArtPropCollector.get( "name" ); //$NON-NLS-1$
      final Feature BodArtFE = getFeature( asciiBodArtId, m_conf.getBodartFT() );

      bodArtPropCollector.put( "soilLayerLink", BodArtFE.getId() ); //$NON-NLS-1$
      bodArtParameterFeature.setProperty( NaModelConstants.PARA_SOIL_LAYER_LINK, BodArtFE.getId() );

      // Collection collection = bodArtPropCollector.values();
      setParsedProperties( bodArtParameterFeature, bodArtPropCollector, null );

      final IPropertyType pt = m_bodentypFT.getProperty( NaModelConstants.PARA_SOIL_LAYER_PARAMETER_MEMBER );
      FeatureHelper.addProperty( feature, pt, bodArtParameterFeature );
    }

    // continue reading
    // Collection collection = propCollector.values();
    setParsedProperties( feature, propCollector, null );
    return feature;
  }

  public void writeFile( final AsciiBuffer asciiBuffer, final GMLWorkspace paraWorkspace ) throws Exception
  {
    final List<Feature> paramSoiltypeLayers = (List<Feature>) paraWorkspace.getRootFeature().getProperty( NaModelConstants.PARA_SOILTYPE_MEMBER );
    for( final Feature paramSoiltypeLayer : paramSoiltypeLayers )
    {
      final List<Feature> bodartList = (List<Feature>) paramSoiltypeLayer.getProperty( NaModelConstants.PARA_SOIL_LAYER_PARAMETER_MEMBER );
      final Map<String, Double[]> layers = new LinkedHashMap<String, Double[]>();
      for( final Feature fe : bodartList )
      {
        final Feature bodArtLink = paraWorkspace.resolveLink( fe, (IRelationType) fe.getFeatureType().getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ) );
        if( bodArtLink != null )
        {
          Boolean xretProp = (Boolean) fe.getProperty( NaModelConstants.PARA_PROP_XRET );
          if( xretProp == null )
            xretProp = Boolean.FALSE;
//            throw new SimulationException( "Parameter WS: Property xretProp is null for feature " + paramSoiltypeLayer.getId() );
          layers.put( bodArtLink.getName(), new Double[] { Double.parseDouble( fe.getProperty( NaModelConstants.PARA_PROP_XTIEF ).toString() ), xretProp ? 1.0 : 0.0 } );
        }
        else
          Logger.getAnonymousLogger().log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.BodentypManager.29", paramSoiltypeLayer.getId(), fe.getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ) ) ); //$NON-NLS-1$
      }
      final String layerName = paramSoiltypeLayer.getName();
      if( !m_soilTypes.containsKey( layerName ) )
        m_soilTypes.put( layerName, layers );
    }
    addSudsSoilLayers();

    final StringBuffer buffer = asciiBuffer.getBodtypBuffer();

    buffer.append( "/Bodentypen:\n/\n/Typ       Tiefe[dm]\n" ); //$NON-NLS-1$
    final Iterator<String> soilTypesIterator = m_soilTypes.keySet().iterator();
    while( soilTypesIterator.hasNext() )
    {
      final String soilType = soilTypesIterator.next();
      final Map<String, Double[]> layers = m_soilTypes.get( soilType );
      buffer.append( String.format( Locale.US, "%-10s%4d\n", soilType, layers.size() ) ); //$NON-NLS-1$
      final Iterator<String> layersIterator = layers.keySet().iterator();
      while( layersIterator.hasNext() )
      {
        final String layer = layersIterator.next();
        final Double[] values = layers.get( layer );
        buffer.append( String.format( Locale.US, "%-8s%.1f %.1f\n", layer, values[0], values[1] ) ); //$NON-NLS-1$
      }
    }
  }

  /**
   * Adds the suds soil types to the existing set. If the type with the same name was already defined, it will be
   * overwritten.
   */
  private final void addSudsSoilLayers( )
  {
    // add Greenroof type
    final Map<String, Double[]> grsLayers = new LinkedHashMap<String, Double[]>();
    grsLayers.put( "GR-stau", new Double[] { 2.0, 0.0 } ); //$NON-NLS-1$
    grsLayers.put( "Substr", new Double[] { 2.0, 0.0 } ); //$NON-NLS-1$
    grsLayers.put( "Drain", new Double[] { 1.0, 0.0 } ); //$NON-NLS-1$
    m_soilTypes.put( "grs", grsLayers ); //$NON-NLS-1$

    final Map<String, Double> mrsTypes = new LinkedHashMap<String, Double>();
    // default is 4.0
    mrsTypes.put( "mrs", 4.0 ); //$NON-NLS-1$
    mrsTypes.put( "mrs_30", 3.0 ); //$NON-NLS-1$
    mrsTypes.put( "mrs_60", 6.0 ); //$NON-NLS-1$
    mrsTypes.put( "mrs_80", 8.0 ); //$NON-NLS-1$

    final Map<String, Double> muldeTypes = new LinkedHashMap<String, Double>();
    // default is 4.0
    muldeTypes.put( "mulde_b", 4.0 ); //$NON-NLS-1$
    muldeTypes.put( "mulde_30", 3.0 ); //$NON-NLS-1$
    muldeTypes.put( "mulde_60", 6.0 ); //$NON-NLS-1$
    muldeTypes.put( "mulde_80", 8.0 ); //$NON-NLS-1$

    // add Mulde-Rigole types
    for( final String typeName : mrsTypes.keySet() )
    {
      final Map<String, Double[]> layers = new LinkedHashMap<String, Double[]>();
      layers.put( "mulde", new Double[] { mrsTypes.get( typeName ), 0.0 } ); //$NON-NLS-1$
      layers.put( "rein", new Double[] { 3.0, 0.0 } ); //$NON-NLS-1$
      layers.put( "filter", new Double[] { 7.0, 0.0 } ); //$NON-NLS-1$
      layers.put( "base", new Double[] { 1.0, 0.0 } ); //$NON-NLS-1$
      m_soilTypes.put( typeName, layers );
    }

    // add Mulde types
    for( final String typeName : muldeTypes.keySet() )
    {
      final Map<String, Double[]> layers = new LinkedHashMap<String, Double[]>();
      layers.put( "mulde", new Double[] { muldeTypes.get( typeName ), 0.0 } ); //$NON-NLS-1$
      layers.put( "rein", new Double[] { 3.0, 0.0 } ); //$NON-NLS-1$
      m_soilTypes.put( typeName, layers );
    }
  }

  /**
   * Returns the predefined soil type name for given swale
   */
  public static final String getSwaleSoiltypeName( final IAbstractSwale swale ) throws Exception
  {
    final Double profileThickness = swale.getProfileThickness();
    if( swale instanceof ISwale )
    {
      if( profileThickness == null )
        return "mulde_b";
      if( profileThickness == 3.0 )
        return "mulde_30";
      if( profileThickness == 6.0 )
        return "mulde_60";
      if( profileThickness == 8.0 )
        return "mulde_80";
      return "mulde_b";
    }
    if( swale instanceof ISwaleInfiltrationDitch )
    {
      if( profileThickness == null )
        return "mrs";
      if( profileThickness == 3.0 )
        return "mrs_30";
      if( profileThickness == 6.0 )
        return "mrs_60";
      if( profileThickness == 8.0 )
        return "mrs_80";
      return "mrs";
    }
    throw new Exception( "Unknown swale type, class: " + swale.getClass().getCanonicalName() );
  }
}
