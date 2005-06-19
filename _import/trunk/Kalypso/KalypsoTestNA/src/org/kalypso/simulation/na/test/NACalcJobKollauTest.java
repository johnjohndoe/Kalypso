package org.kalypso.simulation.na.test;

import java.io.File;
import java.net.URL;

import javax.xml.bind.JAXBException;

import junit.framework.TestCase;

import org.kalypso.convert.namodel.NaModelCalcJob;
import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypso.convert.namodel.NaModelInnerCalcJob;
import org.kalypso.convert.namodel.schema.UrlCatalogNA;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.net.IUrlCatalog;
import org.kalypso.java.net.MultiUrlCatalog;
import org.kalypso.ogc.gml.typehandler.DiagramTypeHandler;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree_impl.extension.ITypeRegistry;
import org.kalypsodeegree_impl.extension.TypeRegistryException;
import org.kalypsodeegree_impl.extension.TypeRegistrySingleton;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCatalog;
import org.kalypsodeegree_impl.gml.schema.schemata.DeegreeUrlCatalog;

import com.sun.rsasign.s;

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

public class NACalcJobKollauTest extends TestCase
{

  public void testKollau() throws TypeRegistryException, JAXBException, CalcJobServiceException
  {

    final IUrlCatalog catalog = new MultiUrlCatalog( new IUrlCatalog[]
    {
        new DeegreeUrlCatalog(),
        new UrlCatalogNA() } );
    GMLSchemaCatalog.init( catalog, FileUtilities.createNewTempDir( "schemaCache" ) );

    final ITypeRegistry registry = TypeRegistrySingleton.getTypeRegistry();
    registry.registerTypeHandler( new ObservationLinkHandler() );
    registry.registerTypeHandler( new DiagramTypeHandler() );
    try
    {
      //      weisseElster();
      //      weisseElsterOptimize();
      kollau();
      //      kollauOptimize();
    }
    catch( CalcJobServiceException e )
    {
      e.printStackTrace();
      throw e;
    }
  }

  private File getTmpDir()
  {
    File file = FileUtilities.createNewTempDir( "NA_TEST", new File( "C:\\tmp" ) );
    file.mkdirs();
    return file;
  }

  public void weisseElster() throws CalcJobServiceException
  {
    final File tmp = getTmpDir();

    final ICalcDataProvider dataProvider = new ICalcDataProvider()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#getURLForID(java.lang.String)
       */
      public URL getURLForID( String id )
      {
        Class clazz = getClass();
        if( NaModelConstants.IN_CONTROL_ID.equals( id ) )
          return clazz.getResource( "weisseElster/expertControl.gml" );
        if( NaModelConstants.IN_MODELL_ID.equals( id ) )
          return clazz.getResource( "weisseElster/calcCase.gml" );
        if( NaModelConstants.IN_META_ID.equals( id ) )
          return clazz.getResource( "weisseElster/.calculation" );
        if( NaModelConstants.IN_TEMPLATE_ID.equals( id ) )
          return clazz.getResource( "weisseElster/.asciitemplate_we.zip" );
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#hasID(java.lang.String)
       */
      public boolean hasID( String id )
      {
        return getURLForID( id ) != null;
      }
    };

    final ICalcResultEater resultEater = createResultEater();
    final ICalcMonitor monitor = createMonitor();
    final NaModelInnerCalcJob job = new NaModelInnerCalcJob();
    job.run( tmp, dataProvider, resultEater, monitor );
    assertTrue( job.isSucceeded() );
  }

  public void weisseElsterOptimize() throws CalcJobServiceException
  {
    final File tmp = getTmpDir();

    final ICalcDataProvider dataProvider = new ICalcDataProvider()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#getURLForID(java.lang.String)
       */
      public URL getURLForID( String id )
      {
        Class clazz = getClass();
        if( NaModelConstants.IN_CONTROL_ID.equals( id ) )
          return clazz.getResource( "weisseElster/.nacontrol_4.gml" );
        if( NaModelConstants.IN_MODELL_ID.equals( id ) )
          return clazz.getResource( "weisseElster/calcCase.gml" );
        if( NaModelConstants.IN_META_ID.equals( id ) )
          return clazz.getResource( "weisseElster/.calculation" );
        if( NaModelConstants.IN_TEMPLATE_ID.equals( id ) )
          return clazz.getResource( "weisseElster/.asciitemplate_we.zip" );
        if( NaModelConstants.IN_OPTIMIZECONF_ID.equals( id ) )
          return clazz.getResource( "weisseElster/.sce.xml" );
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#hasID(java.lang.String)
       */
      public boolean hasID( String id )
      {
        return getURLForID( id ) != null;
      }
    };

    final ICalcResultEater resultEater = createResultEater();
    final ICalcMonitor monitor = createMonitor();
    final ICalcJob job = new NaModelCalcJob();
    job.run( tmp, dataProvider, resultEater, monitor );
  }

  public void kollau() throws CalcJobServiceException
  {
    final File tmp = getTmpDir();
    System.out.println( "Berechnungsverzeichnis: " + tmp.getPath() );

    final ICalcDataProvider dataProvider = new ICalcDataProvider()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#getURLForID(java.lang.String)
       */
      public URL getURLForID( String id )
      {
        Class clazz = getClass();
        if( NaModelConstants.IN_CONTROL_ID.equals( id ) )
          return clazz.getResource( "kollau/expertControl.gml" );
        if( NaModelConstants.IN_MODELL_ID.equals( id ) )
          return clazz.getResource( "kollau/calcCase_kollau.gml" );
        // 
        //        if( NaModelConstants.IN_MODELL_ID.equals( id ) )
        //          return clazz.getResource( "kollau/calcCase_cycle.gml" );
        if( NaModelConstants.IN_META_ID.equals( id ) )
          return clazz.getResource( "kollau/.calculation" );
        if( NaModelConstants.IN_TEMPLATE_ID.equals( id ) )
          return clazz.getResource( "kollau/.asciitemplate.zip" );
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#hasID(java.lang.String)
       */
      public boolean hasID( String id )
      {
        return getURLForID( id ) != null;
      }
    };

    final ICalcResultEater resultEater = createResultEater();
    final ICalcMonitor monitor = createMonitor();
    final NaModelInnerCalcJob job = new NaModelInnerCalcJob();
    job.run( tmp, dataProvider, resultEater, monitor );
    assertTrue( job.isSucceeded() );
  }

  public void kollauOptimize() throws CalcJobServiceException
  {

    final NaModelCalcJob job = new NaModelCalcJob();
    final File tmp = getTmpDir();

    final ICalcDataProvider dataProvider = new ICalcDataProvider()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#getURLForID(java.lang.String)
       */
      public URL getURLForID( String id )
      {
        Class clazz = getClass();
        if( NaModelConstants.IN_CONTROL_ID.equals( id ) )
          return clazz.getResource( "kollau/.nacontrol_1.gml" );
        if( NaModelConstants.IN_MODELL_ID.equals( id ) )
          return clazz.getResource( "kollau/calcCase.gml" );
        if( NaModelConstants.IN_META_ID.equals( id ) )
          return clazz.getResource( "kollau/.calculation" );
        if( NaModelConstants.IN_TEMPLATE_ID.equals( id ) )
          return clazz.getResource( "kollau/.asciitemplate.zip" );
        if( NaModelConstants.IN_OPTIMIZECONF_ID.equals( id ) )
          return clazz.getResource( "kollau/.sce.xml" );
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#hasID(java.lang.String)
       */
      public boolean hasID( String id )
      {
        return getURLForID( id ) != null;
      }
    };

    final ICalcResultEater resultEater = createResultEater();
    final ICalcMonitor monitor = createMonitor();

    job.run( tmp, dataProvider, resultEater, monitor );
  }

  private ICalcResultEater createResultEater()
  {
    return new ICalcResultEater()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#addResult(java.lang.String,
       *      java.io.File)
       */
      public void addResult( String id, File file )
      {
        System.out.println( "ID" + id + " File:" + file.getAbsolutePath() );
      }
    };
  }

  private ICalcMonitor createMonitor()
  {
    return new ICalcMonitor()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#cancel()
       */
      public void cancel()
      {
      //  
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#isCanceled()
       */
      public boolean isCanceled()
      {
        return false;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#setProgress(int)
       */
      public void setProgress( int progress )
      {
      //
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#getProgress()
       */
      public int getProgress()
      {
        return 0;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#getMessage()
       */
      public String getMessage()
      {
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#setMessage(java.lang.String)
       */
      public void setMessage( String message )
      {
        System.out.println( message + "\n" );
      }

      public void setFinishInfo( int status, String text )
      {
      // TODO Auto-generated method stub

      }

      public String getFinishText()
      {
        // TODO Auto-generated method stub
        return null;
      }

      public int getFinishStatus()
      {
        // TODO Auto-generated method stub
        return 0;
      }
    };

  }
}
