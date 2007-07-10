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
package test.org.kalypso.kalypsomodel1d2d;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.math.BigInteger;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Date;
import java.util.GregorianCalendar;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IPath;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaCatalog;
import org.kalypso.gmlschema.KalypsoGMLSchemaPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IResultModelDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ISimulationDescriptor;
import org.kalypso.kalypsomodel1d2d.schema.binding.metadata.ResultDB;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

import test.org.kalypso.kalypsosimulationmodel.TestUtils;

/**
 * 
 * @author Patrice Congo
 * 
 */
public class TestResultDB extends TestCase
{

  public void testPluginCreation( )
  {
    final KalypsoModel1D2DPlugin plugin = KalypsoModel1D2DPlugin.getDefault();
    assertNotNull( plugin.getResultDB() );
  }

  public void testCreation( )
  {
    try
    {
      final GMLSchemaCatalog schemaCatalog = KalypsoGMLSchemaPlugin.getDefault().getSchemaCatalog();
      final String namespaceURI = Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR_COLLECTION.getNamespaceURI();
      final URL urlMetaSchemaNS = FileLocator.resolve( schemaCatalog.getDefaultCatalog().getURL( namespaceURI ) );
      final String gmlVersion = "3.1.1";
      System.out.println( "SCHEMA:" + schemaCatalog.getSchema( namespaceURI, gmlVersion ) );
      final GMLSchema createdSchema = schemaCatalog.getSchema( gmlVersion, urlMetaSchemaNS );
      System.out.println( "Created Schema:" + createdSchema );
      System.out.println( "schemaURL:" + urlMetaSchemaNS );
      System.out.println( "NAmespace:" + namespaceURI );
      final IPath folder = ResultDB.getFolder();
      System.out.println( folder );
      final ResultDB resultDB = new ResultDB( folder );
      final IFeatureWrapperCollection<ISimulationDescriptor> simulationDescriptors = resultDB.getSimulationDescriptors();
      final ISimulationDescriptor simDescriptor = simulationDescriptors.addNew( Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR );

      final IModelDescriptor modelDesc = resultDB.addModelDescriptor( simulationDescriptors );
      resultDB.save();
    }
    catch( final Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
  }

  /**
   * Test model descriptor type; create, modify, reload, and compare
   */
  public void testModelDescriptor( )
  {

    try
    {
      final QName descQName = Kalypso1D2DSchemaConstants.SIMMETA_F_MODELDESCRIPTOR;
      final String tempFileName = descQName.getLocalPart() + System.currentTimeMillis();
      final File tmpFile = File.createTempFile( tempFileName, "gml" );
      final IModelDescriptor modelDesc = createFeature( tmpFile, descQName, IModelDescriptor.class );

      modelDesc.getWrappedFeature().setProperty( Kalypso1D2DSchemaConstants.GML_PROP_BOUNDED_BY, GeometryFactory.createGM_Envelope( 0, 0, 1, 1 ) );
      saveWorkSpace( modelDesc );
      final IModelDescriptor modelDescReloaded = loadRootFeature( tmpFile, IModelDescriptor.class );
      assertEquals( modelDesc.getGmlID(), modelDescReloaded.getGmlID() );
    }
    catch( final Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
  }

  /**
   * Test result model descriptor type; create, modify, reload, and compare
   */
  public void testResultDescriptor( )
  {
    try
    {
      final QName descQName = Kalypso1D2DSchemaConstants.SIMMETA_F_RESULT;
      final String tempFileName = descQName.getLocalPart() + System.currentTimeMillis();
      final File tmpFile = File.createTempFile( tempFileName, "gml" );
      final IResultModelDescriptor modelDesc = createFeature( tmpFile, descQName, IResultModelDescriptor.class );

      modelDesc.getWrappedFeature().setProperty( Kalypso1D2DSchemaConstants.GML_PROP_BOUNDED_BY, GeometryFactory.createGM_Envelope( 0, 0, 1, 1 ) );
      modelDesc.setTime( new Date() );
      modelDesc.setModelID( "modelID_ididid" );
      modelDesc.setModelName( "modelName_namemmemem" );
      modelDesc.setTimeStepNum( new BigInteger( "1" ) );
      modelDesc.setTinDepth( "_depth" );
      modelDesc.setTinVelocity( "_velocity" );
      modelDesc.setTinWaterLevel( "newValue" );
      modelDesc.setGmt( "_gmt" );

      // GmlSerializer.serializeWorkspace(
      // new OutputStreamWriter(System.out),
      // modelDesc.getWrappedFeature().getWorkspace() );

      saveWorkSpace( modelDesc );
      // File realFile=new File("C:\\Temp\\aaaaTest\\gml_time.txt");
      final IResultModelDescriptor modelDescReloaded = loadRootFeature( tmpFile, IResultModelDescriptor.class );
      assertEquals( modelDesc.getGmlID(), modelDescReloaded.getGmlID() );
      assertEquals( modelDesc.getTime(), modelDescReloaded.getTime() );
      assertEquals( modelDesc.getModelID(), modelDescReloaded.getModelID() );
      assertEquals( modelDesc.getModelType(), modelDescReloaded.getModelType() );
      assertEquals( modelDesc.getTinDepth(), modelDescReloaded.getTinDepth() );
      assertEquals( modelDesc.getTinVelocity(), modelDescReloaded.getTinVelocity() );
      assertEquals( modelDesc.getTinWaterLevel(), modelDescReloaded.getTinWaterLevel() );
      assertEquals( modelDesc.getGmt(), modelDescReloaded.getGmt() );
    }
    catch( final Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
  }

  public void testSimulationDescriptor( )
  {
    try
    {
      final QName descQName = Kalypso1D2DSchemaConstants.SIMMETA_F_SIMDESCRIPTOR;
      final String tempFileName = descQName.getLocalPart() + System.currentTimeMillis();
      final File tmpFile = File.createTempFile( tempFileName, "gml" );
      final ISimulationDescriptor simDesc = createFeature( tmpFile, descQName, ISimulationDescriptor.class );

      simDesc.getWrappedFeature().setProperty( Kalypso1D2DSchemaConstants.GML_PROP_BOUNDED_BY, GeometryFactory.createGM_Envelope( 0, 0, 1, 1 ) );
      simDesc.setStartTime( new GregorianCalendar() );
      simDesc.setEndTime( new GregorianCalendar() );
      simDesc.setRestarted( true );
      simDesc.setScenarioName( "scenarioName_1" );
      simDesc.setAutoconverged( true );
      simDesc.setSimulationType( ISimulationDescriptor.SIMULATIONTYPE.Qsteady );

      // setting state for comparison

      // GmlSerializer.serializeWorkspace(
      // new OutputStreamWriter(System.out),
      // simDesc.getWrappedFeature().getWorkspace() );
      //      
      saveWorkSpace( simDesc );
      final ISimulationDescriptor simDescReloaded = loadRootFeature( tmpFile, ISimulationDescriptor.class );
      assertEquals( simDesc.getGmlID(), simDescReloaded.getGmlID() );
      assertEquals( simDesc.getEndTime(), simDescReloaded.getEndTime() );
      assertEquals( simDesc.getStartTime(), simDescReloaded.getStartTime() );
      assertEquals( simDesc.getScenarioName(), simDescReloaded.getScenarioName() );
      assertEquals( simDesc.isAutoconverged(), simDescReloaded.isAutoconverged() );
      assertEquals( simDesc.isRestarted(), simDescReloaded.isRestarted() );
      assertEquals( simDesc.getSimulationType(), simDescReloaded.getSimulationType() );
    }
    catch( final Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
    }
  }

  /**
   * Saves the feature workspace.
   * 
   * @param feature
   *            the feature which workspace has to be saved
   */
  public static final void saveWorkSpace( final IFeatureWrapper2 feature ) throws FileNotFoundException, GmlSerializeException
  {
    final GMLWorkspace workspace = feature.getWrappedFeature().getWorkspace();
    final OutputStreamWriter writer = new OutputStreamWriter( new FileOutputStream( new File( workspace.getContext().getFile() ) ) );
    GmlSerializer.serializeWorkspace( writer, workspace );
  }

  /**
   * Loads and adapts the root feature of the gml workspace in the given gml file.
   * 
   * @param workspaceFile
   *            the workspace file
   * @return the adapted workflow root feature
   */
  public <T extends IFeatureWrapper2> T loadRootFeature( final File workspaceFile, final Class<T> adapterClass ) throws MalformedURLException, Exception
  {
    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( workspaceFile.toURL(), null );
    final T root = (T) workspace.getRootFeature().getAdapter( adapterClass );
    return root;
  }

  /**
   * Creates new workspace and returns its adapted root feature.
   * 
   * @param emptyFile
   *            the workspace file
   * @param featureQName
   *            the qname of the root feature
   * @param adapterClass
   *            the target adapter file
   * 
   */
  public <T extends IFeatureWrapper2> T createFeature( final File emptyFile, final QName featureQName, final Class<T> adapterClass )
  {

    try
    {
      final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( featureQName, emptyFile.toURL(), GmlSerializer.DEFAULT_FACTORY );
      final T adapted = (T) workspace.getRootFeature().getAdapter( adapterClass );
      return adapted;
    }
    catch( final Exception e )
    {
      fail( TestUtils.getStackTraceAsString( e ) );
      return null;// unreachable
    }
  }

}
