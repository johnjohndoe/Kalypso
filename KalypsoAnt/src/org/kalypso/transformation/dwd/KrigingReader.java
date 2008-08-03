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
package org.kalypso.transformation.dwd;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBElement;

import org.kalypso.ogc.sensor.filter.FilterFactory;
import org.kalypso.zml.filters.AbstractFilterType;
import org.kalypso.zml.filters.NOperationFilterType;
import org.kalypso.zml.filters.OperationFilterType;
import org.kalypso.zml.filters.ZmlFilterType;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypso.zml.repository.virtual.ItemType;
import org.kalypso.zml.repository.virtual.LevelType;
import org.kalypso.zml.repository.virtual.VirtualRepositoryType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.w3._1999.xlinkext.SimpleLinkType;

public class KrigingReader
{
  private final String doublePattern = "[0-9\\.]+";

  // TODO remove member ?
  private final org.kalypso.zml.repository.virtual.ObjectFactory vRepFac = new org.kalypso.zml.repository.virtual.ObjectFactory();

  // example: BLOCK: 4480066.00 5560799.00 1000000.00 1 1 1
  private final Pattern BLOCK = Pattern.compile( ".*BLOCK:.+?(" + doublePattern + ").+?(" + doublePattern + ").+" );

  // example: 4485832.000 5603328.000 0.420 4234
  private final Pattern RELATION = Pattern.compile( ".*?(" + doublePattern + ") +?(" + doublePattern + ") +?(" + doublePattern + ") +(.+?) *" );

  private final List<KrigingElement> m_krigingElements;

  private int m_min = 9999999;

  private final Logger m_logger;

  private final SourceObservationProvider m_srcObservationProvider;

  private final String m_crs;

  public KrigingReader( Logger logger, Reader reader, SourceObservationProvider srcObservationProvider, String crs ) throws IOException
  {
    m_logger = logger;
    m_srcObservationProvider = srcObservationProvider;
    m_crs = crs;
    m_krigingElements = parse( reader );
    reader.close();
  }

  public AbstractFilterType createFilter( final Feature feature, final String geoPropName )
  {
    m_logger.info( "creatFilter for " + feature.getId() );
    final GM_Object geom = (GM_Object) feature.getProperty( geoPropName );
    final List elements = getKrigingElementsFor( geom );
    if( elements.isEmpty() )
      throw new InvalidParameterException( "Raster ist zu grob, Keine zuordnung fuer " + feature.getId() + " gefunden.\n" );
    return createFilter( elements );
  }

  private AbstractFilterType createFilter( List krigingElements )
  {
    if( krigingElements.size() < m_min )
      m_min = krigingElements.size();
    m_logger.info( krigingElements.size() + " rasterpoints are withing geometry. (min is" + m_min + ")" );
    // calculate dependency
    final HashMap<String, KrigingRelation> map = new HashMap<String, KrigingRelation>();
    final double n = krigingElements.size();
    // loop elements
    for( Iterator iter = krigingElements.iterator(); iter.hasNext(); )
    {
      KrigingElement ke = (KrigingElement) iter.next();
      KrigingRelation[] relations = ke.getRelations();
      // loop relations
      for( int i = 0; i < relations.length; i++ )
      {
        KrigingRelation relation = relations[i];
        final String id = relation.getId();
        final double factor = relation.getFactor() / n;
        if( !map.containsKey( id ) )
          map.put( id, new KrigingRelation( factor, id ) );
        else
        {
          KrigingRelation rel = map.get( id );
          rel.setFactor( rel.getFactor() + factor );
        }
      }
    }

    final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();
    final NOperationFilterType nOperationFilter = FilterFactory.OF_FILTER.createNOperationFilterType();
    nOperationFilter.setOperator( "+" );
    final List<JAXBElement< ? extends AbstractFilterType>> filterList = nOperationFilter.getFilter();
    for( Iterator iter = map.values().iterator(); iter.hasNext(); )
    {
      final OperationFilterType filter = FilterFactory.OF_FILTER.createOperationFilterType();
      filterList.add( FilterFactory.OF_FILTER.createFilter( filter ) );
      final KrigingRelation rel = (KrigingRelation) iter.next();
      filter.setOperator( "*" );
      filter.setOperand( Double.toString( rel.getFactor() ) );
      final ZmlFilterType zmlLink = FilterFactory.OF_FILTER.createZmlFilterType();
      final SimpleLinkType type = linkFac.createSimpleLinkType();
      final TimeseriesLinkType srcObservaion = m_srcObservationProvider.getObservaionForId( rel.getId() );
      type.setHref( srcObservaion.getHref() );
      // type.setHref( DWD_PSI_Mapper.mapDWDtoPSI( rel.getId() ) );
      zmlLink.setZml( type );
      filter.setFilter( FilterFactory.OF_FILTER.createFilter( zmlLink ) );
      m_logger.info( rel.getId() + " " + rel.getFactor() );
    }

    return nOperationFilter;
  }

  private List<KrigingElement> getKrigingElementsFor( final GM_Object geom )
  {
    final List<KrigingElement> result = new ArrayList<KrigingElement>();
    for( final Iterator iter = m_krigingElements.iterator(); iter.hasNext(); )
    {
      final KrigingElement ke = (KrigingElement) iter.next();
      if( geom.contains( ke.getCenterPoint() ) )
        result.add( ke );
    }
    return result;
  }

  public List<KrigingElement> parse( Reader reader )
  {
    final List<KrigingElement> result = new ArrayList<KrigingElement>();
    try
    {
      // // TODO check coordinates system
      // final CS_CoordinateSystem srs =
      // ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
      final LineNumberReader lineReader = new LineNumberReader( reader );
      String line = null;
      KrigingElement e = null;
      while( (line = lineReader.readLine()) != null )
      {
        Matcher m1 = BLOCK.matcher( line );
        Matcher m2 = RELATION.matcher( line );
        if( m1.matches() )
        {
          double x = Double.parseDouble( m1.group( 1 ) );
          double y = Double.parseDouble( m1.group( 2 ) );
          e = new KrigingElement( GeometryFactory.createGM_Point( x, y, m_crs ) );
          result.add( e );
        }
        else if( m2.matches() )
        {
          final double factor = Double.parseDouble( m2.group( 3 ) );
          final String id = m2.group( 4 );
          if( e != null )
            e.addRelation( factor, id );
        }
      }
      lineReader.close();
    }
    catch( NumberFormatException e1 )
    {
      e1.printStackTrace();
    }
    catch( IOException e1 )
    {
      e1.printStackTrace();
    }
    return result;
  }

  public VirtualRepositoryType createRepositoryConf( Feature[] features, String geoPropName )
  {
    final VirtualRepositoryType repository = vRepFac.createVirtualRepositoryType();
    final LevelType level = vRepFac.createLevelType();
    level.setId( "Messung" );
    level.setName( "WeisseElster - Gebietsniederschlaege" );
    repository.getLevel().add( level );

    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      final ItemType item = vRepFac.createItemType();
      item.setId( feature.getId() );
      item.setName( "Niederschlag - " + feature.getId() );
      item.setFilter( FilterFactory.OF_FILTER.createFilter( createFilter( feature, geoPropName ) ) );
      level.getItem().add( item );
    }
    return repository;
  }
}