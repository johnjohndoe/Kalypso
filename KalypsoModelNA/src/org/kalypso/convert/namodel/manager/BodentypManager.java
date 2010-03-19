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
import org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof.EUsageType;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
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

  private final Map<String, List<Layer>> m_soilTypes = new LinkedHashMap<String, List<Layer>>();

  private final class Layer
  {
    private final String m_name;

    private final double m_thickness;

    private final boolean m_interflow;

    public Layer( final String name, final double thickness, final boolean interflow )
    {
      m_name = name;
      m_thickness = thickness;
      m_interflow = interflow;
    }

    public final String getName( )
    {
      return m_name;
    }

    public final double getThickness( )
    {
      return m_thickness;
    }

    public final boolean interflow( )
    {
      return m_interflow;
    }
  }

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
      final List<Layer> layers = new ArrayList<Layer>();
      for( final Feature fe : bodartList )
      {
        final Feature bodArtLink = paraWorkspace.resolveLink( fe, (IRelationType) fe.getFeatureType().getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ) );
        if( bodArtLink != null )
        {
          Boolean xretProp = (Boolean) fe.getProperty( NaModelConstants.PARA_PROP_XRET );
          if( xretProp == null )
            xretProp = Boolean.FALSE;
// throw new SimulationException( "Parameter WS: Property xretProp is null for feature " + paramSoiltypeLayer.getId() );
          final Layer layer = new Layer( bodArtLink.getName(), Double.parseDouble( fe.getProperty( NaModelConstants.PARA_PROP_XTIEF ).toString() ), xretProp );
          layers.add( layer );
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
      final List<Layer> layers = m_soilTypes.get( soilType );
      buffer.append( String.format( Locale.US, "%-10s%4d\n", soilType, layers.size() ) ); //$NON-NLS-1$
      for( final Layer layer : layers )
      {
        buffer.append( String.format( Locale.US, "%-8s%.1f %.1f\n", layer.getName(), layer.getThickness(), layer.interflow() ? 1.0 : 0.0 ) ); //$NON-NLS-1$
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
    final List<Layer> greenroofExternalLayers = new ArrayList<Layer>();
    greenroofExternalLayers.add( new Layer( "GR-stau", 2.0, false ) ); //$NON-NLS-1$
    greenroofExternalLayers.add( new Layer( "Substr", 2.0, false ) ); //$NON-NLS-1$
    greenroofExternalLayers.add( new Layer( "Drain", 1.0, false ) ); //$NON-NLS-1$
    m_soilTypes.put( EUsageType.EXTENSIVE.getSoilTypeID(), greenroofExternalLayers ); //$NON-NLS-1$

    final List<Layer> greenroofInternalLayers = new ArrayList<Layer>();
    greenroofInternalLayers.add( new Layer( "GR-stau", 2.0, false ) ); //$NON-NLS-1$
    greenroofInternalLayers.add( new Layer( "Substr", 6.0, false ) ); //$NON-NLS-1$
    greenroofInternalLayers.add( new Layer( "Drain", 1.0, false ) ); //$NON-NLS-1$
    m_soilTypes.put( EUsageType.INTENSIVE.getSoilTypeID(), greenroofInternalLayers ); //$NON-NLS-1$

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
      final List<Layer> layers = new ArrayList<Layer>();
      layers.add( new Layer( "mulde", mrsTypes.get( typeName ), false ) ); //$NON-NLS-1$
      layers.add( new Layer( "rein", 3.0, false ) ); //$NON-NLS-1$
      layers.add( new Layer( "filter", 7.0, false ) ); //$NON-NLS-1$
      layers.add( new Layer( "base", 1.0, false ) ); //$NON-NLS-1$
      m_soilTypes.put( typeName, layers );
    }

    // add Mulde types
    for( final String typeName : muldeTypes.keySet() )
    {
      final List<Layer> layers = new ArrayList<Layer>();
      layers.add( new Layer( "mulde", muldeTypes.get( typeName ), false ) ); //$NON-NLS-1$
      layers.add( new Layer( "rein", 3.0, false ) ); //$NON-NLS-1$
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
      if( profileThickness == 0.3 )
        return "mulde_30";
      if( profileThickness == 0.6 )
        return "mulde_60";
      if( profileThickness == 0.8 )
        return "mulde_80";
      return "mulde_b";
    }
    if( swale instanceof ISwaleInfiltrationDitch )
    {
      if( profileThickness == null )
        return "mrs";
      if( profileThickness == 0.3 )
        return "mrs_30";
      if( profileThickness == 0.6 )
        return "mrs_60";
      if( profileThickness == 0.8 )
        return "mrs_80";
      return "mrs";
    }
    throw new Exception( "Unknown swale type, class: " + swale.getClass().getCanonicalName() );
  }
}
