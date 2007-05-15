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
package org.kalypso.services.calculation.service.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.Map;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jws.WebService;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.AbstractUrlCatalog;
import org.kalypso.contribs.java.net.ClassUrlCatalog;
import org.kalypso.contribs.java.net.IUrlCatalog;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.core.RefactorThis;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.ogc.gml.typehandler.ZmlInlineTypeHandler;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.services.calculation.service.CalcJobServerBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.ICalculationService;
import org.kalypso.services.common.ServiceConfig;
import org.kalypsodeegree.model.TypeHandlerUtilities;

/**
 * Exposes the {@link org.kalypso.services.calculation.service.impl.QueuedCalcJobService}suitable as web-service.
 * (default constructor etc.).
 * <p>
 * Reads configuration from file, support logging and register TypeHandler for GML.
 * </p>
 * 
 * @author Belger
 */
@WebService
public class QueuedCalcJobServiceWrapper implements ICalculationService
{
  protected static final Logger LOGGER = Logger.getLogger( QueuedCalcJobServiceWrapper.class.getName() );

  private final ICalculationService m_service;

  public QueuedCalcJobServiceWrapper( ) throws RemoteException
  {
    // Logger initialisieren
    try
    {
      LOGGER.addHandler( new FileHandler( ServiceConfig.getTempDir() + "/" + ClassUtilities.getOnlyClassName( ICalculationService.class ) + "%g.log", 10000000, 10, true ) );
    }
    catch( final IOException e ) // generic Exception caught for simplicity
    {
      throw new RemoteException( "LOGGER des Rechdienst konnte nicht initialisiert werden", e );
    }

    try
    {
      final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();

      TypeHandlerUtilities.registerTypeHandlers( registry );
      TypeHandlerUtilities.registerXSDSimpleTypeHandler( registry );

      RefactorThis.registerSpecialTypeHandler( registry );

      // TODO refactor this
      final ZmlInlineTypeHandler wvqInline = new ZmlInlineTypeHandler( "ZmlInlineWVQType", ZmlInlineTypeHandler.WVQ.axis, ZmlInlineTypeHandler.WVQ.class );
      final ZmlInlineTypeHandler taInline = new ZmlInlineTypeHandler( "ZmlInlineTAType", ZmlInlineTypeHandler.TA.axis, ZmlInlineTypeHandler.TA.class );
      final ZmlInlineTypeHandler wtKcLaiInline = new ZmlInlineTypeHandler( "ZmlInlineIdealKcWtLaiType", ZmlInlineTypeHandler.WtKcLai.axis, ZmlInlineTypeHandler.WtKcLai.class );
      registry.registerTypeHandler( wvqInline );
      registry.registerTypeHandler( taInline );
      registry.registerTypeHandler( wtKcLaiInline );

      registry.registerTypeHandler( wvqInline );
      registry.registerTypeHandler( taInline );

    }
    catch( final Exception e )
    {
      throw new RemoteException( "GML Typ-Handler konnten nicht registriert werden.", e );
    }

    LOGGER.info( "Rechendienst wird gestartet" );
    LOGGER.info( "Lese Konfigurationsdateien" );

    // die root aus dem Kalypso-Server-Properties lesen
    InputStream confStream = null;
    URL classCatalogUrl = null;
    int maxThreads = 1;
    long schedulingPeriod = 2000;
    ICalcJobFactory factory = new PropertyCalcJobFactory( new Properties() );
    try
    {
      final URL confLocation = ServiceConfig.getConfLocation();
      final URL myConfUrl = UrlResolverSingleton.resolveUrl( confLocation, ClassUtilities.getOnlyClassName( ICalculationService.class ) + "/" );

      // Konfiguration der Modelltypen
      final URL typeUrl = UrlResolverSingleton.resolveUrl( myConfUrl, "modelltypen.properties" );
      // if( !typeFile.exists() )
      // throw new RemoteException( "Can't find configuration file: " + typeFile.getAbsolutePath() );
      factory = new PropertyCalcJobFactory( typeUrl );

      // Konfiguration dieser Service-Implementation
      final Properties confProps = new Properties();
      final URL confUrl = UrlResolverSingleton.resolveUrl( myConfUrl, "calculationService.properties" );
      confStream = confUrl.openStream();
      confProps.load( confStream );
      maxThreads = Integer.parseInt( confProps.getProperty( "MAX_THREADS", "1" ) );
      schedulingPeriod = Long.parseLong( confProps.getProperty( "SCHEDULING_PERIOD", "2000" ) );
      final String classCatalogName = confProps.getProperty( "CLASS_CATALOG", null );
      if( classCatalogName != null )
        classCatalogUrl = UrlResolverSingleton.resolveUrl( myConfUrl, classCatalogName );
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      LOGGER.warning( "Could not load service configuration file.\nWill proceed with default values" );
    }
    finally
    {
      IOUtils.closeQuietly( confStream );
    }

    {
      final AbstractUrlCatalog emptyCatalog = new AbstractUrlCatalog()
      {
        @Override
        protected void fillCatalog( final Class myClass, final Map<String, URL> katalog, Map<String, String> prefixes )
        {
          // nix, ist leer
        }
      };

      IUrlCatalog catalog;
      if( classCatalogUrl == null )
        catalog = emptyCatalog;
      else
      {
        try
        {
          catalog = new ClassUrlCatalog( classCatalogUrl );
        }
        catch( final IOException e )
        {
          LOGGER.log( Level.SEVERE, "Fehler beim Lesen des UrlClassCatalog: " + classCatalogUrl.toString(), e );
          catalog = emptyCatalog;
        }
      }

      // TODO: auch den catalog aus der schemaConf nehmen?
      final File cacheDir = new File( FileUtilities.TMP_DIR, "schemaCache" );
      cacheDir.mkdir();

      m_service = new QueuedCalcJobService( factory, catalog, maxThreads, schedulingPeriod );
    }

    LOGGER.info( "Service initialisiert mit:\nMAX_THREAD = " + maxThreads + "\nSCHEDULING_PERIOD = " + schedulingPeriod );
    if( classCatalogUrl == null )
      LOGGER.warning( "Kein Klassen-URL-Katalog angegeben (CLASS_CATALOG). Rechendienst ist vermutlich nicht richtig initialisiert." );
    else
      LOGGER.info( "CLASS_CATALOG = " + classCatalogUrl.toString() );
  }

  /**
   * @throws RemoteException
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion( ) throws RemoteException
  {
    return m_service.getServiceVersion();
  }

  public synchronized final String[] getJobTypes( ) throws CalcJobServiceException
  {
    return m_service.getJobTypes();
  }

  public synchronized CalcJobInfoBean[] getJobs( ) throws CalcJobServiceException
  {
    return m_service.getJobs();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#cancelJob(java.lang.String)
   */
  public void cancelJob( final String jobID ) throws CalcJobServiceException
  {
    m_service.cancelJob( jobID );

    LOGGER.info( "Job canceled: " + jobID );
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#disposeJob(java.lang.String)
   */
  public void disposeJob( final String jobID ) throws CalcJobServiceException
  {
    m_service.disposeJob( jobID );

    LOGGER.info( "Job disposed: " + jobID );
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#startJob(java.lang.String, java.lang.String,
   *      javax.activation.DataHandler,
   *      org.kalypso.services.calculation.service.CalcJobClientBean[],org.kalypso.services.calculation.service.CalcJobClientBean[])
   */
  public final CalcJobInfoBean startJob( final String typeID, final String description, final DataHandler zipHandler, final CalcJobClientBean[] input, final CalcJobClientBean[] output ) throws CalcJobServiceException
  {
    return m_service.startJob( typeID, description, zipHandler, input, output );
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#transferCurrentResults(java.lang.String)
   */
  public DataHandler transferCurrentResults( final String jobID ) throws CalcJobServiceException
  {
    return m_service.transferCurrentResults( jobID );
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getCurrentResults(java.lang.String)
   */
  public String[] getCurrentResults( String jobID ) throws CalcJobServiceException
  {
    return m_service.getCurrentResults( jobID );
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getRequiredInput(java.lang.String)
   */
  public CalcJobServerBean[] getRequiredInput( final String typeID ) throws CalcJobServiceException
  {
    return m_service.getRequiredInput( typeID );
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getDeliveringResults(java.lang.String)
   */
  public CalcJobServerBean[] getDeliveringResults( final String typeID ) throws CalcJobServiceException
  {
    return m_service.getDeliveringResults( typeID );
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getSchema(java.lang.String)
   */
  public DataHandler getSchema( final String namespace ) throws CalcJobServiceException
  {
    return m_service.getSchema( namespace );
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getSchemaValidity(java.lang.String)
   */
  public long getSchemaValidity( final String namespace ) throws CalcJobServiceException
  {
    return m_service.getSchemaValidity( namespace );
  }

  /**
   * @throws CalcJobServiceException
   * @see org.kalypso.services.calculation.service.ICalculationService#getSupportedSchemata()
   */
  public String[] getSupportedSchemata( ) throws CalcJobServiceException
  {
    return m_service.getSupportedSchemata();
  }

  /**
   * @see org.kalypso.services.calculation.service.ICalculationService#getJob(java.lang.String)
   */
  public CalcJobInfoBean getJob( String jobID ) throws CalcJobServiceException
  {
    return m_service.getJob( jobID );
  }
}