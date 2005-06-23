//package org.kalypso.simulation.na.test;
//
//import java.io.File;
//import java.io.FileWriter;
//
//import junit.framework.TestCase;
//
//import org.apache.commons.io.FileUtils;
//import org.kalypsodeegree.model.feature.Feature;
//import org.kalypsodeegree.model.feature.FeatureProperty;
//import org.kalypsodeegree.model.feature.FeatureType;
//import org.kalypsodeegree.model.feature.GMLWorkspace;
//import org.kalypsodeegree_impl.extension.ITypeRegistry;
//import org.kalypsodeegree_impl.extension.MarshallingTypeRegistrySingleton;
//import org.kalypsodeegree_impl.model.feature.FeatureFactory;
//import org.kalypso.convert.namodel.NaModelCalcJob;
//import org.kalypso.convert.namodel.NaModelConstants;
//import org.kalypso.convert.namodel.NaModelInnerCalcJob;
//import org.kalypso.convert.namodel.schema.KalypsoNADefaultSchema;
//import org.kalypso.contribs.java.io.FileCopyVisitor;
//import org.kalypso.contribs.java.io.FileUtilities;
//import org.kalypso.ogc.gml.serialize.GmlSerializer;
//import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
//import org.kalypso.services.calculation.common.ICalcServiceConstants;
//import org.kalypso.services.calculation.job.impl.CalcJobHelper;
//import org.kalypso.services.calculation.service.CalcJobDataBean;
//
///*----------------    FILE HEADER KALYPSO ------------------------------------------
// *
// *  This file is part of kalypso.
// *  Copyright (C) 2004 by:
// * 
// *  Technical University Hamburg-Harburg (TUHH)
// *  Institute of River and coastal engineering
// *  Denickestraﬂe 22
// *  21073 Hamburg, Germany
// *  http://www.tuhh.de/wb
// * 
// *  and
// *  
// *  Bjoernsen Consulting Engineers (BCE)
// *  Maria Trost 3
// *  56070 Koblenz, Germany
// *  http://www.bjoernsen.de
// * 
// *  This library is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU Lesser General Public
// *  License as published by the Free Software Foundation; either
// *  version 2.1 of the License, or (at your option) any later version.
// * 
// *  This library is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// *  Lesser General Public License for more details.
// * 
// *  You should have received a copy of the GNU Lesser General Public
// *  License along with this library; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// * 
// *  Contact:
// * 
// *  E-Mail:
// *  belger@bjoernsen.de
// *  schlienger@bjoernsen.de
// *  v.doemming@tuhh.de
// *   
// *  ---------------------------------------------------------------------------*/
//
//public class KalypsoNAAcidTest extends TestCase
//{
//
//  public void testRun() throws Exception
//  {
//    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
//    registry.registerTypeHandler( new ObservationLinkHandler() );
//
//    final File inputDir = new File( "C:\\Programme\\KalypsoServer\\data\\tmp\\TEST" );
//
//    final File baseInputDir = new File( inputDir, ICalcServiceConstants.INPUT_DIR_NAME );
//    final CalcJobDataBean modelBean = new CalcJobDataBean( NaModelConstants.IN_MODELL_ID,
//        "Modelldaten", "calc/calcCase.gml" );
//    final File modelFile = new File( baseInputDir, modelBean.getPath() );
//
//    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( modelFile.toURL(),
//        KalypsoNADefaultSchema.getDefaultNaModellSchemaURL() );
//    final FeatureType nodeFT = workspace.getFeatureType( "Node" );
//    final Feature[] features = workspace.getFeatures( nodeFT );
//    StringBuffer failBuffer = new StringBuffer();
//    // first test the difficult
//    //    if( !run( inputDir, "Node7002", false ) )
//    //      fail( "failed initial" );
//    //    if( !run( inputDir, "Node1001", false ) )
//    //      fail( "failed initial" );
//
//    for( int i = 0; i < features.length; i++ )
//    {
//      final Feature nodeFE = features[i];
//      boolean success = run( inputDir, nodeFE.getId(), true );
//      if( !success )
//        failBuffer.append( " simulation for " + nodeFE.getId() + " failed\n" );
//    }
//    System.out.println(failBuffer.toString());
//    if( failBuffer.length() > 0 )
//      fail( failBuffer.toString() );
//  }
//
//  private boolean run( File inputDir, String rootNode, boolean deleteOnSuccess ) throws Exception
//  {
//    final File baseDir = FileUtilities.createNewTempDir( rootNode + "_NaAcidTest" );
//    final FileCopyVisitor copyVisitor = new FileCopyVisitor( inputDir, baseDir, true );
//    FileUtilities.accept( inputDir, copyVisitor ,true);
//
//    final File simDir = new File( baseDir, "sim" );
//    final File ergDir = new File( baseDir, "output" );
//    if( simDir.exists() )
//      FileUtils.cleanDirectory( simDir );
//    if( ergDir.exists() )
//      FileUtils.cleanDirectory( ergDir );
//
//    final CalcJobDataBean[] beans = new CalcJobDataBean[]
//    {
//        new CalcJobDataBean( NaModelConstants.IN_MODELL_ID, "Modelldaten", "calc/calcCase.gml" ),
//        new CalcJobDataBean( NaModelConstants.IN_CONTROL_ID, "Steuerdaten", "calc/.nacontrol.gml" ),
//        new CalcJobDataBean( NaModelConstants.IN_META_ID, "MetaSteuerdaten", "calc/.calculation" ),
//        new CalcJobDataBean( "NiederschlagDir", "niederschlag", "calc/Niederschlag/" ),
//        new CalcJobDataBean( "ZuflussDir", "zufluesse", "calc/Zufluss/" ),
//        new CalcJobDataBean( "PegelDir", "pegel", "calc/Pegel/" ) };
//
//    // modify control
//    final File baseInputDir = new File( baseDir, ICalcServiceConstants.INPUT_DIR_NAME );
//    final CalcJobDataBean controlBean = CalcJobHelper.getBeanForId( NaModelInnerCalcJob.IN_CONTROL_ID,
//        beans );
//    final File controlFile = new File( baseInputDir, controlBean.getPath() );
//    final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( controlFile.toURL(),
//        KalypsoNADefaultSchema.getDefaultNaControlSchemaURL() );
//    final Feature controlFE = workspace.getRootFeature();
//    FeatureProperty property = FeatureFactory.createFeatureProperty( "rootNode", rootNode );
//    controlFE.setProperty( property );
//    FileWriter writer = new FileWriter( controlFile );
//    GmlSerializer.serializeWorkspace( writer, workspace );
//    writer.close();
//    final NaModelCalcJob job = new NaModelCalcJob();
//    job.run( baseDir, beans );
//    if( deleteOnSuccess && job.isSucceeded() )
//      FileUtils.deleteDirectory( baseDir );
//    return job.isSucceeded();
//  }
//}