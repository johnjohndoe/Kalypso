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
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.i18n.Messages;
import org.kalypso.convert.namodel.schema.binding.Hydrotop;
import org.kalypso.convert.namodel.schema.binding.suds.AbstractSud;
import org.kalypso.convert.namodel.schema.binding.suds.Greenroof;
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
    final List<Greenroof> greenroofs = new ArrayList<Greenroof>();
    final List<AbstractSud> suds = (List<AbstractSud>) m_conf.getSudsWorkspace().getRootFeature().getProperty( new QName( "http://sourceforge.kalypso.org/schemata/hydrology/suds", "sudMember" ) );
    for( final AbstractSud sudsItem : suds )
      if( sudsItem instanceof Greenroof )
        greenroofs.add( (Greenroof) sudsItem );

    final List<Hydrotop> hydrotops = (List<Hydrotop>) m_conf.getHydrotopeWorkspace().getRootFeature().getProperty( NaModelConstants.HYDRO_MEMBER );

    final Feature rootFeature = paraWorkspace.getRootFeature();
    final List<Feature> list = (List<Feature>) rootFeature.getProperty( NaModelConstants.PARA_SOILTYPE_MEMBER );
    asciiBuffer.getBodtypBuffer().append( "/Bodentypen:\n/\n/Typ       Tiefe[dm]\n" ); //$NON-NLS-1$
    final Iterator<Feature> iter = list.iterator();
    final List<String> names = new ArrayList<String>();
    while( iter.hasNext() )
    {
      final Feature bodentypFE = iter.next();
      final String bodenTypName = bodentypFE.getName();
      names.add( bodenTypName );
      // TODO: nur die schreiben, die auch in Hydrotopdatei vorkommen
      // if( asciiBuffer.writeFeature( bodentypFE ) )
      writeFeature( asciiBuffer, paraWorkspace, bodentypFE );
    }
    
    // needed for suds, fixed values temporarily...
    /*
      mrs          4
      mulde   4.0 0.0 
      rein    3.0 0.0 
      filter  7.0 0.0 
      base    1.0 0.0 
      grs          3
      GR-stau 2.0 0.0 
      Substr  2.0 0.0 
      Drain   1.0 0.0
      mulde_b      2
      mulde   4.0 0.0 
      rein    3.0 0.0    
     */
    if( !names.contains( "mrs" ) )
      asciiBuffer.getBodtypBuffer().append( "mrs          4\nmulde   4.0 0.0\nrein    3.0 0.0\nfilter  7.0 0.0\nbase    1.0 0.0\n" );
    if( !names.contains( "grs" ) )
      asciiBuffer.getBodtypBuffer().append( "grs          3\nGR-stau 2.0 0.0\nSubstr  2.0 0.0\nDrain   1.0 0.0\n" );
    if( !names.contains( "mulde_b" ) )
      asciiBuffer.getBodtypBuffer().append( "mulde_b      2\nmulde   4.0 0.0\nrein    3.0 0.0\n" );
  }

  private void writeFeature( final AsciiBuffer asciiBuffer, final GMLWorkspace paraWorkspace, final Feature feature ) throws Exception
  {
    final StringBuffer buffer = asciiBuffer.getBodtypBuffer();
    // 1
    final List<Feature> bodartList = (List<Feature>) feature.getProperty( NaModelConstants.PARA_SOIL_LAYER_PARAMETER_MEMBER );
    buffer.append( String.format(Locale.US, "%-10s%4d\n", feature.getName(), bodartList.size() ) ); //$NON-NLS-1$
    // 2
    final Iterator<Feature> iter = bodartList.iterator();
    while( iter.hasNext() )
    {
      final Feature fe = iter.next();

      final Feature bodArtLink = paraWorkspace.resolveLink( fe, (IRelationType) fe.getFeatureType().getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ) );
      if( bodArtLink != null )
      {
        final Boolean xretProp = (Boolean) fe.getProperty( NaModelConstants.PARA_PROP_XRET );
        if( xretProp )
          buffer.append( String.format(Locale.US, "%-8s%.3f 1.0\n", bodArtLink.getName(), Double.parseDouble( fe.getProperty( NaModelConstants.PARA_PROP_XTIEF ).toString() ) ) ); //$NON-NLS-1$
        else
          buffer.append( String.format(Locale.US, "%-8s%.3f 0.0\n", bodArtLink.getName(), Double.parseDouble( fe.getProperty( NaModelConstants.PARA_PROP_XTIEF ).toString() ) ) ); //$NON-NLS-1$
      }
      else
      {
        Logger.getAnonymousLogger().log( Level.WARNING, Messages.getString( "org.kalypso.convert.namodel.manager.BodentypManager.29", feature.getId(), fe.getProperty( NaModelConstants.PARA_SOIL_LAYER_LINK ) ) ); //$NON-NLS-1$
      }
    }
  }
}
