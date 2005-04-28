/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of ekalypso:
 Internet based elearning for complex simulation applications
 ("Internet basiertes E-Learning an komplexen Simulationsprogrammen [de]")

 The implementation is realised by: 
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 The project is sponsored and supported by:  
 local authority of education and research, 
 E-Learning Consortium Hamburg (ELCH) and
 Multimedia Kontor Hamburg.

 As this implementation depends on third party open source 
 java code it is consequently also licenced as open source
 in the hope that it will be useful specially (but not exclusively)
 to other e-learning projects that may extend or use this project.

 Copyright (C) 2004, 2005 by:
 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

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
 katharina.lupp@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
/*
 * 
 * Created on 23.03.2005
 *  
 */
package org.kalypso.calc2d.test;

import java.io.File;
import java.net.URL;
import java.util.HashMap;

import javax.activation.DataHandler;

import junit.framework.TestCase;

import org.apache.commons.io.FileUtils;
import org.kalypso.calc2d.CalcJob2d;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcMonitor;
import org.kalypso.services.calculation.job.ICalcResultEater;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualFeatureTypeRegistry;
import org.kalypsodeegree_impl.gml.schema.virtual.VirtualVelocityFeatureTypePropertyHandler;

/**
 * 
 * -----------------------------------------------------------------
 * 
 * @author katharina lupp <a href="mailto:k.lupp@web.de>Katharina Lupp </a>
 *  
 */

public class CalcJob2dTest extends TestCase
{
  public void test2dModell() throws Exception
  {
    VirtualFeatureTypeRegistry vRegistry = VirtualFeatureTypeRegistry.getInstance();
    vRegistry.register(new VirtualVelocityFeatureTypePropertyHandler());
    
    ICalcDataProvider dataProvider = new ICalcDataProvider()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#getURLForID(java.lang.String)
       */
      public URL getURLForID( String id ) throws CalcJobServiceException
      {
        if( CalcJob2d.MODELL_ID.equals( id ) )
          return getClass().getResource( "calcCaseResultMesh.gml" );
        else if( CalcJob2d.CONTROL_ID.equals( id ) )
          return getClass().getResource( "boundaryConditions.gml" );
        throw new CalcJobServiceException( "resource id=" + id + " not found", null );
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcDataProvider#hasID(java.lang.String)
       */
      public boolean hasID( String id )
      {
        return ( CalcJob2d.MODELL_ID.equals( id ) || CalcJob2d.CONTROL_ID.equals( id ) );
      }
    };

    ICalcResultEater resultEater = new ICalcResultEater()
    {
      final HashMap map = new HashMap();

      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#addResult(java.lang.String,
       *      java.io.File)
       */
      public void addResult( String id, File file )
      {
        map.put( id, file );
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#getCurrentResults()
       */
      public String[] getCurrentResults()
      {
        return (String[])map.keySet().toArray( new String[map.size()] );
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#packCurrentResults()
       */
      public DataHandler packCurrentResults()
      {
        return null;
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#addFile(java.io.File)
       */
      public void addFile( File file )
      {
      // 
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcResultEater#disposeFiles()
       */
      public void disposeFiles()
      {
      //        
      }
    };

    ICalcMonitor monitor = new ICalcMonitor()
    {
      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#cancel()
       */
      public void cancel()
      {
      //       do nothing
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#isCanceled()
       */
      public boolean isCanceled()
      {
        return false; // never in test
      }

      /**
       * @see org.kalypso.services.calculation.job.ICalcMonitor#setProgress(int)
       */
      public void setProgress( int progress )
      {
        System.out.println( "Progress: " + progress );
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
        System.out.println( message );
      }
    };
    try
    {
      final ICalcJob job = new CalcJob2d();
      final File tmpDir = new File( "C:\\Temp\\2DCalcTest" );
      tmpDir.mkdirs();
      FileUtils.cleanDirectory( tmpDir );
      job.run( tmpDir, dataProvider, resultEater, monitor );
      //      if( job.isSucceeded() )
      //        System.out.println( "berechnung ohne Fehler beendet :-)" );
      //      else
      //        System.out.println( ":-( fehler irgendwo" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }
    //  
  }
}