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

package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.binding.parameter.SoilLayerParameter;
import org.kalypso.model.hydrology.binding.parameter.Soiltype;
import org.kalypso.model.hydrology.binding.suds.IAbstractSwale;
import org.kalypso.model.hydrology.binding.suds.IGreenRoof.EUsageType;
import org.kalypso.model.hydrology.binding.suds.ISwale;
import org.kalypso.model.hydrology.binding.suds.ISwaleInfiltrationDitch;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author huebsch
 */
public class BodentypWriter extends AbstractCoreFileWriter
{
  private final GMLWorkspace m_parameterWorkspace;

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

    public String getName( )
    {
      return m_name;
    }

    public double getThickness( )
    {
      return m_thickness;
    }

    public boolean interflow( )
    {
      return m_interflow;
    }
  }

  public BodentypWriter( final GMLWorkspace parameterWorkspace, final Logger logger )
  {
    super( logger );

    m_parameterWorkspace = parameterWorkspace;
  }

  @Override
  protected void writeContent( final PrintWriter buffer )
  {
    final Map<String, List<Layer>> soilTypes = collectSoilTypes();

    buffer.append( "/Bodentypen:\n/\n/Typ       Tiefe[dm]\n" ); //$NON-NLS-1$

    final Set<Entry<String, List<Layer>>> entrySet = soilTypes.entrySet();
    for( final Entry<String, List<Layer>> entry : entrySet )
    {
      final String soilType = entry.getKey();
      final List<Layer> layers = entry.getValue();

      buffer.append( String.format( Locale.US, "%-10s%4d\n", soilType, layers.size() ) ); //$NON-NLS-1$

      for( final Layer layer : layers )
        buffer.append( String.format( Locale.US, "%-8s%.1f %.1f\n", layer.getName(), layer.getThickness(), layer.interflow() ? 1.0 : 0.0 ) ); //$NON-NLS-1$
    }
  }

  private Map<String, List<Layer>> collectSoilTypes( )
  {
    final Parameter parameter = (Parameter) m_parameterWorkspace.getRootFeature();
    final IFeatureBindingCollection<Soiltype> soiltypes = parameter.getSoiltypes();

    final Map<String, List<Layer>> soilTypes = new LinkedHashMap<>( soiltypes.size() );

    for( final Soiltype soiltype : soiltypes )
    {
      final List<Layer> layers = new ArrayList<Layer>();

      final List<SoilLayerParameter> bodartList = soiltype.getParameters();
      for( final SoilLayerParameter layerParameter : bodartList )
      {
        final Feature bodArtLink = layerParameter.getMember( SoilLayerParameter.LINK_SOIL_LAYER );
        if( bodArtLink != null )
        {
          final boolean xret = layerParameter.getXRet();
          final double xtief = layerParameter.getXTief();
          final Layer layer = new Layer( bodArtLink.getName(), xtief, xret );
          layers.add( layer );
        }
        else
        {
          final Object linkRef = layerParameter.getProperty( SoilLayerParameter.LINK_SOIL_LAYER );
          Logger.getAnonymousLogger().log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.BodentypManager.29", soiltype.getId(), linkRef ) ); //$NON-NLS-1$
        }
      }

      final String layerName = soiltype.getName();
      if( !soilTypes.containsKey( layerName ) )
        soilTypes.put( layerName, layers );
    }
    addSudsSoilLayers( soilTypes );

    return soilTypes;
  }

  /**
   * Adds the suds soil types to the existing set. If the type with the same name was already defined, it will be
   * overwritten.
   */
  private final void addSudsSoilLayers( final Map<String, List<Layer>> soilTypes )
  {
    // add Greenroof type
    final List<Layer> greenroofExternalLayers = new ArrayList<Layer>();
    greenroofExternalLayers.add( new Layer( "GR-stau", 1.35, false ) ); //$NON-NLS-1$
    greenroofExternalLayers.add( new Layer( "Substr", 0.8, false ) ); //$NON-NLS-1$
    greenroofExternalLayers.add( new Layer( "Drain", 0.5, false ) ); //$NON-NLS-1$
    soilTypes.put( EUsageType.EXTENSIVE.getSoilTypeID(), greenroofExternalLayers ); //$NON-NLS-1$

    final List<Layer> greenroofInternalLayers = new ArrayList<Layer>();
    greenroofInternalLayers.add( new Layer( "GR-stau", 1.7, false ) ); //$NON-NLS-1$
    greenroofInternalLayers.add( new Layer( "Substr", 2.0, false ) ); //$NON-NLS-1$
    greenroofInternalLayers.add( new Layer( "Drain", 1.2, false ) ); //$NON-NLS-1$
    soilTypes.put( EUsageType.INTENSIVE.getSoilTypeID(), greenroofInternalLayers ); //$NON-NLS-1$

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
      layers.add( new Layer( "filter", 6.0, false ) ); //$NON-NLS-1$
      layers.add( new Layer( "base", 1.0, false ) ); //$NON-NLS-1$
      soilTypes.put( typeName, layers );
    }

    // add Mulde types
    for( final String typeName : muldeTypes.keySet() )
    {
      final List<Layer> layers = new ArrayList<Layer>();
      layers.add( new Layer( "mulde", muldeTypes.get( typeName ), false ) ); //$NON-NLS-1$
      layers.add( new Layer( "rein", 3.0, false ) ); //$NON-NLS-1$
      layers.add( new Layer( "basem", 3.0, false ) ); //$NON-NLS-1$
      soilTypes.put( typeName, layers );
    }
  }

  /**
   * Returns the predefined soil type name for given swale
   */
  public static final String getSwaleSoiltypeName( final IAbstractSwale swale ) throws NAPreprocessorException
  {
    final Double profileThickness = swale.getProfileThickness();
    if( swale instanceof ISwale )
    {
      if( profileThickness == null )
        return "mulde_b"; //$NON-NLS-1$
      if( profileThickness == 0.3 )
        return "mulde_30"; //$NON-NLS-1$
      if( profileThickness == 0.6 )
        return "mulde_60"; //$NON-NLS-1$
      if( profileThickness == 0.8 )
        return "mulde_80"; //$NON-NLS-1$
      return "mulde_b"; //$NON-NLS-1$
    }
    if( swale instanceof ISwaleInfiltrationDitch )
    {
      if( profileThickness == null )
        return "mrs"; //$NON-NLS-1$
      if( profileThickness == 0.3 )
        return "mrs_30"; //$NON-NLS-1$
      if( profileThickness == 0.6 )
        return "mrs_60"; //$NON-NLS-1$
      if( profileThickness == 0.8 )
        return "mrs_80"; //$NON-NLS-1$
      return "mrs"; //$NON-NLS-1$
    }
    throw new NAPreprocessorException( "Unknown swale type, class: " + swale.getClass().getCanonicalName() ); //$NON-NLS-1$
  }
}
