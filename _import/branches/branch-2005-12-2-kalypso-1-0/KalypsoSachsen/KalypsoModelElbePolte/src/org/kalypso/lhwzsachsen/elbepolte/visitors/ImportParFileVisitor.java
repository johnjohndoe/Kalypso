/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.lhwzsachsen.elbepolte.visitors;

import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.StringTokenizer;

import org.apache.commons.io.IOUtils;
import org.kalypso.lhwzsachsen.elbepolte.ElbePolteConst;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.IPropertiesFeatureVisitor;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * 
 * visitor to import native PAR file into modell.gml (used in *.gmc)
 * 
 * @author thuel2
 */
public class ImportParFileVisitor implements IPropertiesFeatureVisitor
{

  /**
   * imports ObereElbe.Par into modell.gml, modifiying strecken
   */
  private static final class Strecke
  {
    private final String m_streckeNr;
    private final String m_streckeName;
    private final String m_streckeInfo;
    private List m_streckeParams = new ArrayList( 3 );

    public Strecke( final String nr, final String name, final String info, final List params )
    {
      m_streckeNr = nr;
      m_streckeName = name;
      m_streckeInfo = info;
      m_streckeParams = params;
    }

    public String getStreckeNr()
    {
      return m_streckeNr;
    }

    public String getStreckeName()
    {
      return m_streckeName;
    }

    public String getStreckeInfo()
    {
      return m_streckeInfo;
    }

    public List getStreckeParams()
    {
      return m_streckeParams;
    }
  }

  private static final class StreckeParams
  {
    private final String m_hwTypeInfo;
    private List m_hwTypeParams = new ArrayList( 3 );

    public StreckeParams( final String hwInfo, final List hwParams )
    {
      m_hwTypeInfo = hwInfo;
      m_hwTypeParams = hwParams;
    }

    public String getHwTypeInfo()
    {
      return m_hwTypeInfo;
    }

    public List getHwTypeParams()
    {
      return m_hwTypeParams;
    }
  }

  /** streckeNr -> List&lt;Strecke&gt; */
  private final Map m_streckeHash = new HashMap();

  private String m_paramPath;
  private String m_context;
  private Collection m_exceptions = new ArrayList();

  /**
   * @see org.kalypsodeegree.model.feature.IPropertiesFeatureVisitor#init(java.util.Properties)
   */
  public void init( Properties properties ) 
  {
    m_paramPath = properties.getProperty( "paramPath", null );

    m_context = properties.getProperty( "context", null ); // property "context" is set automatically when creating the
    // visitor

    URL contextUrl = null;
    LineNumberReader rdrParams = null;
    try
    {
      contextUrl = new URL( m_context );
      final URL paramUrl = new URL( contextUrl, m_paramPath );

      //    ObereElbe.PAR öffnen (zum Lesen)
      rdrParams = new LineNumberReader(
          new InputStreamReader( paramUrl.openStream(), ElbePolteConst.ELBEPOLTE_CODEPAGE ) );
      //    allgemeine Modellparams lesen (erste Zeile)
      //    erste Zeile igonorieren, enthält Modellparams (könnten doch auch gesetzt werden...)
      String sZeile = rdrParams.readLine();

      sZeile = rdrParams.readLine();
      // Strecken generieren
      String streckeNr;
      while( sZeile != null )
      {
        // new "strecke": Strecken - Nr. DB, LT, ZZG, EFM, nrFEKO
        final StringTokenizer zeileTok = new StringTokenizer( sZeile, "/" );
        final String streckeInfo = zeileTok.nextToken();
        final StringTokenizer paramTok = new StringTokenizer( streckeInfo );
        streckeNr = paramTok.nextToken(); // Strecken - Nr.

        // strecke name
        final String streckeName = rdrParams.readLine();

        final List listStreckeParams = new ArrayList( 3 );
        for( int ii = 0; ii < 3; ii++ )
        {
          final String HWParams = new StringTokenizer( rdrParams.readLine(), "/" ).nextToken();
          // read inner params for strecke
          final List innerParams = new ArrayList( 3 );
          for( int jj = 0; jj < 3; jj++ )
          {
            innerParams.add( new StringTokenizer( rdrParams.readLine(), "/" ).nextToken() );
          }
          final StreckeParams streckeParams = new StreckeParams( HWParams, innerParams );
          listStreckeParams.add( streckeParams );
        }

        final Strecke strecke = new Strecke( streckeNr, streckeName, streckeInfo, listStreckeParams );

        m_streckeHash.put( streckeNr, strecke );
        sZeile = rdrParams.readLine();
      }
    }
    catch( Exception e )
    {
      m_exceptions.add( e );
    }
    finally
    {
      IOUtils.closeQuietly( rdrParams );
    }

  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  public boolean visit( Feature f )
  {
    // find nr of strecke
    final String nr = (String)f.getProperty( "nr" );
    // add params for strecke
    addParams( f, nr );

    return false;
  }

  private void addParams( final Feature f, final String nr )
  {

    // put strecke into gml
    final Object objStrecke = m_streckeHash.get( nr );
    if( objStrecke == null )
    {
      System.out.println( "Keine Strecke für NR: " + nr );
      return;
    }
    final Strecke strecke = (Strecke)objStrecke;
    f.setProperty( "name", strecke.getStreckeName() );

    // DB, LT, ZZG, EFM, nrFEKO setzen (StreckeInfo) werden überschrieben
    final String streckeInfo = strecke.getStreckeInfo();
    final StringTokenizer sInfoTok = new StringTokenizer( streckeInfo );
    sInfoTok.nextToken(); // Strecken-Nummer überspringen
    f.setProperty( "db", sInfoTok.nextToken() );
    f.setProperty( "lt", sInfoTok.nextToken() );
    f.setProperty( "zwg_zuschlag", sInfoTok.nextToken() );
    final String nextToken = sInfoTok.nextToken();
    f.setProperty( "replaceValues", "1".equals( nextToken ) ? "true" : "false" );
    f.setProperty( "nr_feko", sInfoTok.nextToken() );

    final String sParamSetMem = "paramSetMember";
    final FeatureList list = (FeatureList)f.getProperty( sParamSetMem );
    final FeatureAssociationTypeProperty ftp = (FeatureAssociationTypeProperty)f.getFeatureType().getProperty(
        sParamSetMem );
    final FeatureType elementType = ftp.getAssociationFeatureType();

    final String sStufenParamSetMem = "stufenParamSetMember";

    // delete old paramSets
    list.clear();

    int count = 0;
    final List streckeParams = strecke.getStreckeParams();
    for( final Iterator iter = streckeParams.iterator(); iter.hasNext(); )
    {

      // neues element StreckeParams
      final StreckeParams sParams = (StreckeParams)iter.next();
      final StringTokenizer hwTypInfoTok = new StringTokenizer( sParams.getHwTypeInfo() );
      final String elementId = f.getId() + "-params-" + count++;

      // neues Feature: 'paramSetMember'
      final Feature element = FeatureFactory.createFeature( elementId, elementType, new Object[]
      {
          //          hw_Typ, QL1, QL2, km, ce, is, stufenParamSetMember

          Integer.toString( count ),
          hwTypInfoTok.nextToken(),
          hwTypInfoTok.nextToken(),
          hwTypInfoTok.nextToken(),
          hwTypInfoTok.nextToken(),
          hwTypInfoTok.nextToken(),
          null

      } );

      final FeatureAssociationTypeProperty ftpSet = (FeatureAssociationTypeProperty)element.getFeatureType()
          .getProperty( sStufenParamSetMem );
      final FeatureType elementTypeSet = ftpSet.getAssociationFeatureType();

      FeatureList listSet = (FeatureList)element.getProperty( sStufenParamSetMem );
      if( listSet != null )
      {
        listSet.clear();
      }
      else
      {
        listSet = FeatureFactory.createFeatureList( element, ftpSet );
        element.setProperty( sStufenParamSetMem, listSet );
      }

      final List hwTypeParams = sParams.getHwTypeParams();
      int countSet = 0;
      for( final Iterator iter2 = hwTypeParams.iterator(); iter2.hasNext(); )
      {
        // neues element stufenParamSet
        final String hwTypeParamSet = (String)iter2.next();
        final StringTokenizer paramSetTok = new StringTokenizer( hwTypeParamSet );

        final String elementSetId = element.getId() + "-paramSet-" + countSet++;
        // neues Feature: 'paramSetMember'
        final String tl = paramSetTok.nextToken();
        if( !"-1".equals( tl ) )
        {
          final int bCount = Integer.parseInt( paramSetTok.nextToken() );
          final String a1 = paramSetTok.nextToken();
          final String a2 = paramSetTok.nextToken();
          final String[] bWerte = new String[bCount];
          for( int ii = 0; ii < bCount; ii++ )
          {
            bWerte[ii] = paramSetTok.nextToken();
          }

          Feature elementSet;
          if( paramSetTok.hasMoreTokens() )
          {
            elementSet = FeatureFactory.createFeature( elementSetId, elementTypeSet, new Object[]
            {
                tl,
                a1,
                a2,
                null,
                paramSetTok.nextToken() } );
          }
          else
          {
            elementSet = FeatureFactory.createFeature( elementSetId, elementTypeSet, new Object[]
            {
                tl,
                a1,
                a2,
                null,
                null } );
          }

          final String sBMem = "bMember";
          final FeatureAssociationTypeProperty ftpBMem = (FeatureAssociationTypeProperty)elementSet.getFeatureType()
              .getProperty( sBMem );
          final FeatureType ftBMem = ftpBMem.getAssociationFeatureType();

          FeatureList bList = (FeatureList)elementSet.getProperty( sBMem );
          if( bList != null )
          {
            bList.clear();
          }
          else
          {
            bList = FeatureFactory.createFeatureList( elementSet, ftpBMem );
            elementSet.setProperty( sBMem, bList );
          }

          for( int ii = 0; ii < bCount; ii++ )
          {
            final String bMemberId = elementSetId + "-bMember-" + ii;
            final Feature b = FeatureFactory.createFeature( bMemberId, ftBMem, new Object[]
            { bWerte[ii] } );

            bList.add( b );
          }
          listSet.add( elementSet );
        }
      }
      list.add( element );
    }
  }

  public boolean hasException()
  {
    return !m_exceptions.isEmpty();
  }

  public Exception[] getExceptions()
  {
    return (Exception[])m_exceptions.toArray( new Exception[m_exceptions.size()] );
  }

}
