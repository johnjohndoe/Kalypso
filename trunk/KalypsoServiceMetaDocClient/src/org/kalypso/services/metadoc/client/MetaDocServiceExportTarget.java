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

package org.kalypso.services.metadoc.client;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.xml.rpc.ServiceException;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.auth.KalypsoAuthPlugin;
import org.kalypso.auth.scenario.IScenario;
import org.kalypso.auth.scenario.ScenarioUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.TempFileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.core.client.KalypsoServiceCoreClientPlugin;
import org.kalypso.core.client.ProxyFactory;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.impl.AbstractExportTarget;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypso.ogc.gml.selection.FeatureSelectionManager2;
import org.kalypso.services.proxy.IMetaDocService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizard.feature.FeaturePage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.Mapper;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * The file-target simply writes the document into a local file.
 * 
 * @author schlienger
 */
public class MetaDocServiceExportTarget extends AbstractExportTarget
{
  public final static String CONF_METADATA = MetaDocServiceExportTarget.class.getName() + ".METADATA";

  public IStatus commitDocument( final IExportableObject document, final Configuration conf,
      final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    File file = null;
    OutputStream outputStream = null;
    try
    {
      monitor.beginTask( "Dokument " + document.getPreferredDocumentName() + " wird exportiert", 2 );

      file = TempFileUtilities.createTempFile( KalypsoGisPlugin.getDefault(), "metadoc", document
          .getPreferredDocumentName(), "tmp" );
      file.deleteOnExit();

      outputStream = new BufferedOutputStream( new FileOutputStream( file ) );

      final IStatus status = document.exportObject( outputStream, new SubProgressMonitor( monitor, 1 ) );
      outputStream.close();

      // in the case of an error, and just in this case, break export. In other cases, continue.
      if( status.matches( IStatus.ERROR ) )
        return status;

      final IMetaDocService metadocService = getMetadocService();

      // tricky: serialize the metadata extensions as a string
      final StringWriter sw = new StringWriter();
      m_metadataExtensions.save( sw );
      sw.close();

      final DataHandler dh = new DataHandler( new FileDataSource( file ) );
      metadocService.commitNewDocument( (Map)conf.getProperty( CONF_METADATA ), dh,
          document.getPreferredDocumentName(), sw.toString() );

      monitor.worked( 1 );

      return status;
    }
    catch( final CoreException e )
    {
      throw e;
    }
    catch( final Exception e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      monitor.done();

      IOUtils.closeQuietly( outputStream );

      if( file != null )
        file.delete();
    }
  }

  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration ) throws CoreException
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoServiceMetaDocClientPlugin
        .getID(), "icons/wizban/bericht_wiz.gif" );

    final Feature feature = prepareFeature( configuration );
    final IWizardPage page = new FeaturePage( "metadocServicePage", "Metadaten für die Dokumentenablage", imgDesc,
        false, null, feature, new FeatureSelectionManager2() )
    {
      protected void applyFeatureChange( FeatureChange fc )
      {
        super.applyFeatureChange( fc );

        final Map metadata = (Map)configuration.getProperty( CONF_METADATA );

        final Object newValue = Mapper.mapJavaValueToXml( fc.getNewValue() );
        metadata.put( fc.getProperty(), newValue );
      }
    };

    return new IWizardPage[]
    { page };
  }

  /**
   * Prepare a feature for being used as data-model in our wizard page
   */
  private static Feature prepareFeature( final IPublishingConfiguration configuration ) throws CoreException
  {
    final KalypsoAuthPlugin authPlugin = KalypsoAuthPlugin.getDefault();

    final String scenarioID = configuration.getString( "currentScenarioId", "" );
    final IScenario scenario = authPlugin.getScenario( scenarioID );
    final Map metadata;
    try
    {
      metadata = getMetadocService().prepareNewDocument( authPlugin.getCurrentUser().getUserName() );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
          "Serverseitige Fehler (Berichtsablage)", e ) );
    }

    configuration.setProperty( CONF_METADATA, metadata );

    // create featuretype from bean
    final Collection ftpColl = new ArrayList();
    final Collection fpColl = new ArrayList();
    final int[] ints = new int[metadata.size()];
    int count = 0;
    for( final Iterator iter = metadata.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();

      final String name = entry.getKey().toString();

      final String[] splits = entry.getValue().toString().split( ";" );

      final String xmltype = splits.length == 0 ? String.class.getName() : splits[0];

      String value = splits.length >= 2 ? splits[1] : null;

      // TRICKY: maybe replace tokens with scenario specific values
      value = ScenarioUtilities.replaceTokens( value, scenario );
      // Important: always put value again in metadata, in case it changed
      metadata.put( name, value );

      String typename = null;
      Object realValue = null;
      try
      {
        typename = Mapper.mapXMLSchemaType2JavaType( xmltype );
        realValue = value == null ? null : Mapper.mapXMLValueToJava( value, typename );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        typename = "java.lang.String";
      }
      ftpColl.add( FeatureFactory.createFeatureTypeProperty( name, typename, false ) );
      fpColl.add( FeatureFactory.createFeatureProperty( name, realValue ) );

      ints[count++] = 1;
    }

    final FeatureTypeProperty[] ftps = (FeatureTypeProperty[])ftpColl.toArray( new FeatureTypeProperty[ftpColl.size()] );
    final FeatureType ft = FeatureFactory.createFeatureType( "docbean", null, ftps, ints, ints, null, new HashMap() );

    return FeatureFactory.createFeature( "0", ft, (FeatureProperty[])fpColl
        .toArray( new FeatureProperty[fpColl.size()] ) );
  }

  /**
   * @return instance of the client proxy of the metadoc service
   * @throws CoreException
   *           if client-server connection could not be established
   */
  public static IMetaDocService getMetadocService() throws CoreException
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoServiceCoreClientPlugin.getDefault().getProxyFactory();
      return (IMetaDocService)serviceProxyFactory.getAnyProxy( "Kalypso_MetaDocService", ClassUtilities
          .getOnlyClassName( IMetaDocService.class ) );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e,
          "Dokumentenablage-Dienst konnte nicht initialisiert werden" ) );
    }
  }
}
