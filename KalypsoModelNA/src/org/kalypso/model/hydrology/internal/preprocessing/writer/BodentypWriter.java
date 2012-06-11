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
import org.kalypso.model.hydrology.internal.i18n.Messages;
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

    return soilTypes;
  }

}
