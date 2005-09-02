/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.lhwsachsenanhalt.saale.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.io.CSV;
import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.IPropertiesFeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * Dieser Besucher liest die Met_Geb.std und schreibt dann an alle besuchten Features die ausgelesenen Gewichte f�r das
 * Gebietsniederschlagsmodell.
 * 
 * @author belger
 */
public class MetGebStdVisitor implements IPropertiesFeatureVisitor
{
  private static final class MetGebGewichtung
  {
    private final double m_weight;
    private final String m_targetId;

    public MetGebGewichtung( final CSV csv, final int row, final int index )
    {
      final String weight = csv.getItem( row, index + 1 ).replace( ',', '.' ).trim();
      m_weight = Double.parseDouble( weight );
      m_targetId = csv.getItem( row, index ).trim();
    }

    public String getTargetId()
    {
      return m_targetId;
    }

    public double getWeight()
    {
      return m_weight;
    }
  }

  /** gebietId -> List&lt;MetGebGewichtung&gt; */
  private final Map m_metGebHash = new HashMap();

  private String m_link;

  /**
   * Initilisiert den Besucher. Verwendete Properties:
   * <dl>
   * <dt>MetGebPath</dt>
   * <dd>URL auf die Met_Geb.std Datei, relativ zum Kontext</dd>
   * <dt>context</dt>
   * <dd>URL des Kontexts zum dem alle anderen URLs aufgel�st werden.</dd>
   * <dt>gewichtungLink</dt>
   * <dd>Link auf die Liste der Gewichtungen.</dd>
   * </dl>*
   * 
   * @throws CoreException
   * 
   * @see org.kalypsodeegree.model.feature.IPropertiesFeatureVisitor#init(java.util.Properties)
   */
  public void init( final Properties properties ) throws CoreException
  {
    final String metGebPath = properties.getProperty( "metGebPath", null );
    final String context = properties.getProperty( "context", null );
    m_link = properties.getProperty( "link" );

    try
    {
      final URL contextUrl = new URL( context );
      final URL metGebUrl = new URL( contextUrl, metGebPath );

      final CSV csv = loadMetGebStd( metGebUrl );
      for( int i = 0, max = csv.getLines(); i < max; i++ )
      {
        final List weights = new ArrayList( 4 );
        weights.add( new MetGebGewichtung( csv, i, 4 ) );
        weights.add( new MetGebGewichtung( csv, i, 6 ) );
        weights.add( new MetGebGewichtung( csv, i, 8 ) );
        weights.add( new MetGebGewichtung( csv, i, 10 ) );
        weights.add( new MetGebGewichtung( csv, i, 12 ) );

        final String id = csv.getItem( i, 0 );
        m_metGebHash.put( id, weights );
      }
    }
    catch( final MalformedURLException e )
    {
      throw new CoreException( StatusUtilities
          .createStatus( IStatus.ERROR, "URL auf die Met_Geb.std konnte nicht erzeugt werden.\nmetGebPath = "
              + metGebPath + "\ncontext = " + context, e ) );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( final Feature f )
  {
    final String pnr = (String)f.getProperty( "PNR" );
    final FeatureList list = (FeatureList)f.getProperty( m_link );
    final FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)f.getFeatureType().getProperty( m_link );
    final FeatureType elementType = ftp.getAssociationFeatureType();

    // put values into gml
    final List metGebList = (List)m_metGebHash.get( pnr );
    if( metGebList == null )
    {
      System.out.println( "Keine Faktoren f�r PNR: " + pnr );
      return false;
    }
    
    // delete old weights
    list.clear();

    int count = 0;
    for( final Iterator iter = metGebList.iterator(); iter.hasNext(); )
    {
      final MetGebGewichtung mgg = (MetGebGewichtung)iter.next();
      final double weight = mgg.getWeight();
      final String targetId = mgg.getTargetId();

      if( targetId == null || targetId.equals( "0" ) )
        continue;

      final String ombrometerHref = "ID" + targetId;
      final String elementId = f.getId() + "-gewicht-" + count++;

      // neues Feature: 'GewichtungElement'
      
      final Feature element = FeatureFactory.createFeature( elementId, elementType, new Object[]
      {
          new Double( weight ),
          ombrometerHref } );

      list.add( element );
    }

    // dont recurse, if not we may descent into the gewichtung-elements
    return false;
  }

  private CSV loadMetGebStd( final URL metGebUrl ) throws CoreException
  {
    final CSV csv = new CSV( "\t", 2, true );

    InputStreamReader reader = null;
    try
    {
      reader = UrlResolverSingleton.getDefault().createReader( metGebUrl );
      final BufferedReader br = new BufferedReader( reader );

      csv.fetch( br );

      reader.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();

      IOUtils.closeQuietly( reader );

      throw new CoreException( StatusUtilities.createStatus( IStatus.ERROR, "Fehler beim Lesen der Met_Geb.std: "
          + metGebUrl.toExternalForm(), e ) );
    }

    return csv;
  }
}
