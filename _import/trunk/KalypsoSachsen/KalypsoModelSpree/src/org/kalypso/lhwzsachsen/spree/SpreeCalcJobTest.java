package org.kalypso.lhwzsachsen.spree;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import junit.framework.TestCase;

import org.deegree_impl.extension.ITypeRegistry;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.model.xml.Modelspec;
import org.kalypso.model.xml.ModelspecType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.model.xml.ModelspecType.InputType;
import org.kalypso.ogc.sensor.deegree.ObservationLinkHandler;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.service.CalcJobDataBean;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.xml.sax.InputSource;

/**
 * @author belger
 */
public class SpreeCalcJobTest extends TestCase
{
  public void testReadTS() throws Exception
  {
    final File resultDir = FileUtilities.createNewTempDir( "SpreeCalcJobTest" );

    final File dbfFile = new File( resultDir, "spree.dbf" );
    final File shpFile = new File( resultDir, "spree.shp" );
    final File shxFile = new File( resultDir, "spree.shx" );
    FileUtilities.makeFileFromStream( false, dbfFile, getClass().getResourceAsStream(
        "test/HW040427.dbf" ) );
    FileUtilities.makeFileFromStream( false, shpFile, getClass().getResourceAsStream(
        "test/HW040427.shp" ) );
    FileUtilities.makeFileFromStream( false, shxFile, getClass().getResourceAsStream(
        "test/HW040427.shx" ) );

    final String inputFilename = dbfFile.getAbsolutePath();
    final int pointindex = inputFilename.lastIndexOf( '.' );
    final String inputFilenameWOext = inputFilename.substring( 0, pointindex );

    final Map map = new HashMap();
    map.put( SpreeCalcJob.DATA_LABEL, "Prognose" );
    map.put( SpreeCalcJob.DATA_STARTDATESTRING, "27.4.2005 12:00" );

    new SpreeCalcJob().writeResultsToFolder( inputFilenameWOext, resultDir, map, null );
    FileUtilities.deleteRecursive( resultDir );
  }

  public void test040427() throws IOException, TypeRegistryException, JAXBException
  {
    final File baseDir = calculateModel( "040427" );
    FileUtilities.deleteRecursive( baseDir );
  }

  public void test040427_neu() throws IOException, TypeRegistryException, JAXBException
  {
    final File baseDir = calculateModel( "040427_neu" );
    FileUtilities.deleteRecursive( baseDir );
  }
  
  public File calculateModel( final String testName ) throws IOException, TypeRegistryException, JAXBException
  {
    final SpreeCalcJob cj = new SpreeCalcJob();

    final File basedir = FileUtilities.createNewTempDir( "Spree-CalcJob-Test" );
    
    final CalcJobDataBean[] input = createInput( testName, basedir );

    cj.run( basedir, input );

    // todo: check output
    final CalcJobDataBean[] output = cj.getResults();
    for( int i = 0; i < output.length; i++ )
    {
      final CalcJobDataBean bean = output[i];
      System.out.println( bean.getPath() );
    }

    cj.disposeJob();

    return basedir;
  }

  private CalcJobDataBean[] createInput( final String testName, final File basedir ) throws IOException, TypeRegistryException, JAXBException
  {
    final ITypeRegistry typeRegistry = TypeRegistrySingleton.getTypeRegistry();
    if( !typeRegistry.hasClassName( TimeseriesLink.class.getName() ) )
      typeRegistry.registerTypeHandler( new ObservationLinkHandler() );
    
    final ObjectFactory of = new ObjectFactory();
    final Unmarshaller unmarshaller = of.createUnmarshaller();

    final String calcCase = "calc";
    final String base = "base";

    final File inputdir = new File( basedir, ICalcServiceConstants.INPUT_DIR_NAME );
    final File inputcalcdir = new File( inputdir, calcCase );
    final File inputbasedir = new File( inputdir, base );

    final Modelspec spec = (Modelspec)unmarshaller.unmarshal( new InputSource( getClass()
        .getResourceAsStream( "resources/modelspec.xml" ) ) );
    final List inputList = spec.getInput();
    
    final List inputBeanList = new ArrayList();
    for( final Iterator iter = inputList.iterator(); iter.hasNext(); )
    {
      final ModelspecType.InputType input = (InputType)iter.next();
      final String inputPath = input.getPath();

      final String path = ( input.isRelativeToCalcCase() ? calcCase : base ) + "/" + inputPath;
      
      final File dir = input.isRelativeToCalcCase() ? inputcalcdir : inputbasedir;
      final File inputfile = new File( dir, inputPath );

      final String inputresource = "test/" + testName + "/input/" + path;
      inputfile.getParentFile().mkdirs();
      
      FileUtilities.makeFileFromStream( false, inputfile, getClass().getResourceAsStream(
          inputresource ) );
      
      inputBeanList.add( new CalcJobDataBean( input.getId(), input.getDescription(), path ) );
    }

    final CalcJobDataBean[] input = (CalcJobDataBean[])inputBeanList.toArray( new CalcJobDataBean[inputBeanList.size()]  );
    return input;
  }
}