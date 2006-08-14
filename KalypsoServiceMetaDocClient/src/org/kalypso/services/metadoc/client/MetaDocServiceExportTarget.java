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
import java.io.ByteArrayOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.namespace.QName;
import javax.xml.rpc.ServiceException;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationUtils;
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
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.client.KalypsoServiceCoreClientPlugin;
import org.kalypso.core.client.ProxyFactory;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.impl.AbstractExportTarget;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.selection.FeatureSelectionManager2;
import org.kalypso.services.metadoc.impl.DocumentBean;
import org.kalypso.services.metadoc.impl.KalypsoMetaDocService;
import org.kalypso.services.metadoc.impl.MetaDocException_Exception;
import org.kalypso.services.metadoc.impl.PrepareBean;
import org.kalypso.services.metadoc.impl.DocumentBean.Metadata;
import org.kalypso.services.metadoc.impl.DocumentBean.MetadataExtensions;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizard.feature.FeaturePage;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * This target uses the MetaDoc Service for storing the documents on the server side.
 * <p>
 * The MetadataExtensions should be prepared before the commitDocument method is called. <br>
 * For instance, following keys should be set with valid values: <br>
 * <ul>
 * <li>currentScenarioId: it contains the current scenario id
 * <li>calcCaseName: the name of the current calccase
 * <li>calcCaseDescription: the description of the current calccase
 * <li>projectName: the name of the current project
 * </ul>
 * These properties are used on the server side to set the XML-Metadata of the document being commited.
 * 
 * @author schlienger
 */
public class MetaDocServiceExportTarget extends AbstractExportTarget
{
  public final static String CONF_METADATA = MetaDocServiceExportTarget.class.getName() + ".METADATA";

  public IStatus commitDocument( final IExportableObject document, final Configuration conf, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    // File file = null;
    ByteArrayOutputStream outputStream = null;
    try
    {
      monitor.beginTask( "Dokument " + document.getPreferredDocumentName() + " wird exportiert", 2 );

      // file = TempFileUtilities.createTempFile( KalypsoGisPlugin.getDefault(), "metadoc", document
      // .getPreferredDocumentName(), "tmp" );
      // file.deleteOnExit();

      // outputStream = new BufferedOutputStream( new FileOutputStream( file ) );
      outputStream = new ByteArrayOutputStream();

      // important: use a BaseConfiguration not a MapConfiguration because of the addProperty() contract.
      final BaseConfiguration confEx = new BaseConfiguration();
      // copy our properties into the metadataExtensions, will be used by the webservice
      ConfigurationUtils.copy( getProperties(), confEx );

      final IStatus status = document.exportObject( new BufferedOutputStream( outputStream ), new SubProgressMonitor( monitor, 1 ), confEx );
      outputStream.close();

      // in the case of an error, and just in this case, break export. In other cases, continue.
      if( status.matches( IStatus.ERROR ) )
        return status;

      // final DataHandler dh = new DataHandler( new FileDataSource( file ) );

      final KalypsoMetaDocService metadocService = getMetadocService();

      // metadocService.commitNewDocument( conf.getProperty( CONF_METADATA ), dh, document.getPreferredDocumentName(),
      // document.getIdentifier(), document.getCategory(), map );

      final Map<Object, Object> confMap = (Map<Object, Object>) conf.getProperty( CONF_METADATA );
      final Set<Map.Entry<Object, Object>> confMapEntries = confMap.entrySet();

      final Metadata md = new Metadata();
      final List<org.kalypso.services.metadoc.impl.DocumentBean.Metadata.Entry> mdEntries = md.getEntry();
      for( Map.Entry<Object, Object> confMapEntry : confMapEntries )
      {
        final org.kalypso.services.metadoc.impl.DocumentBean.Metadata.Entry mdEntry = new org.kalypso.services.metadoc.impl.DocumentBean.Metadata.Entry();
        mdEntry.setKey( confMapEntry.getKey() );
        mdEntry.setValue( confMapEntry.getValue() );
        mdEntries.add( mdEntry );
      }

      // get a map out of the metadata extensions
      final Map<Object, Object> exMap = org.kalypso.metadoc.configuration.ConfigurationUtils.createMap( confEx );
      final Set<Entry<Object, Object>> exMapEntries = exMap.entrySet();

      final MetadataExtensions mdEx = new MetadataExtensions();
      final List<org.kalypso.services.metadoc.impl.DocumentBean.MetadataExtensions.Entry> mdExEntries = mdEx.getEntry();
      for( Entry<Object, Object> exMapEntry : exMapEntries )
      {
        final org.kalypso.services.metadoc.impl.DocumentBean.MetadataExtensions.Entry mdExEntry = new org.kalypso.services.metadoc.impl.DocumentBean.MetadataExtensions.Entry();
        mdExEntry.setKey( exMapEntry.getKey() );
        mdExEntry.setValue( exMapEntry.getValue() );
        mdExEntries.add( mdExEntry );
      }

      final DocumentBean docBean = new DocumentBean();
      docBean.setMetadata( md );
      docBean.setPreferredFilename( document.getPreferredDocumentName() );
      docBean.setDocumentIdentifier( document.getIdentifier() );
      docBean.setDocumentCategory( document.getCategory() );
      docBean.setMetadataExtensions( mdEx );

      metadocService.commitNewDocument( docBean, outputStream.toByteArray() );

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
    }
  }

  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration ) throws CoreException
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoServiceMetaDocClientPlugin.getID(), "icons/wizban/bericht_wiz.gif" );

    final Feature feature = prepareFeature( configuration );
    final IWizardPage page = new FeaturePage( "metadocServicePage", "Metadaten für die Dokumentenablage", imgDesc, false, feature, new FeatureSelectionManager2() )
    {
      @Override
      protected void applyFeatureChange( FeatureChange fc )
      {
        super.applyFeatureChange( fc );

        final Map<Object, Object> metadata = (Map<Object, Object>) configuration.getProperty( CONF_METADATA );

        final Object newValue = org.kalypso.gmlschema.Mapper.mapJavaValueToXml( fc.getNewValue() );
        metadata.put( fc.getProperty(), newValue );
      }
    };

    return new IWizardPage[] { page };
  }

  /**
   * Prepare a feature for being used as data-model in our wizard page
   */
  private Feature prepareFeature( final IPublishingConfiguration configuration ) throws CoreException
  {
    final KalypsoAuthPlugin authPlugin = KalypsoAuthPlugin.getDefault();

    final PrepareBean pBean;
    try
    {
      pBean = getMetadocService().prepareNewDocument( authPlugin.getCurrentUser().getUserName() );
    }
    catch( final MetaDocException_Exception e )
    {
      e.printStackTrace();
      throw new CoreException( new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, "Serverseitige Fehler (Berichtsablage)", e ) );
    }

    final Map<Object, Object> map = new HashMap<Object, Object>();
    configuration.setProperty( CONF_METADATA, map );

    final org.kalypso.services.metadoc.impl.PrepareBean.Metadata metadata = pBean.getMetadata();
    final List<org.kalypso.services.metadoc.impl.PrepareBean.Metadata.Entry> entries = metadata.getEntry();

    final Configuration targetProps = getProperties();

    // create featuretype from bean
    final Collection<IValuePropertyType> ftpColl = new ArrayList<IValuePropertyType>();
    final Map<IPropertyType, Object> fpColl = new LinkedHashMap<IPropertyType, Object>();
    final int[] ints = new int[map.size()];
    int count = 0;
    for( org.kalypso.services.metadoc.impl.PrepareBean.Metadata.Entry entry : entries )
    {
      map.put( entry.getKey(), entry.getValue() );

      final String name = entry.getKey().toString();

      final String[] splits = entry.getValue().toString().split( ";" );

      final String xmltype = splits.length == 0 ? String.class.getName() : splits[0];

      String value = splits.length >= 2 ? splits[1] : null;

      // tricky: overwrite value if it is a marker value
      if( targetProps.containsKey( value ) )
      {
        value = targetProps.getString( value );

        // Important: always put value again in metadata when it changed
        map.put( name, value );
      }

      final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
      QName valueQName = new QName( NS.XSD, xmltype );
      IMarshallingTypeHandler handler = registry.getTypeHandlerForTypeName( valueQName );
      if( handler == null )
      {
        valueQName = new QName( NS.XSD, "string" );
        handler = registry.getTypeHandlerForTypeName( valueQName );
      }
      // String typename = null;
      Object realValue = null;
      try
      {
        realValue = value == null ? null : handler.parseType( value );
        // typename = Mapper.mapXMLSchemaType2JavaType( xmltype );
        // realValue = value == null ? null : Mapper.mapXMLValueToJava( value, typename );
      }
      catch( final Exception e )
      {

        e.printStackTrace();
        // typename = "java.lang.String";
      }
      final IValuePropertyType vpt = GMLSchemaFactory.createValuePropertyType( new QName( "unknown", name ), valueQName, handler, 0, 1, false );
      ftpColl.add( vpt );
      fpColl.put( vpt, realValue );

      ints[count++] = 1;
    }

    final IValuePropertyType[] ftps = ftpColl.toArray( new IValuePropertyType[ftpColl.size()] );
    final IFeatureType ft = GMLSchemaFactory.createFeatureType( new QName( "unknown", "docbean" ), ftps );

    final Feature newFeature = FeatureFactory.createFeature( null, "0", ft, false );
    FeatureHelper.setProperties( newFeature, fpColl );
    return newFeature;
  }

  /**
   * @return instance of the client proxy of the metadoc service
   * @throws CoreException
   *           if client-server connection could not be established
   */
  public static KalypsoMetaDocService getMetadocService( ) throws CoreException
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoServiceCoreClientPlugin.getDefault().getProxyFactory();
      return (KalypsoMetaDocService) serviceProxyFactory.getAnyProxy( "Kalypso_MetaDocService", "IMetaDocService" );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();

      throw new CoreException( StatusUtilities.statusFromThrowable( e, "Dokumentenablage-Dienst konnte nicht initialisiert werden" ) );
    }
  }
}
