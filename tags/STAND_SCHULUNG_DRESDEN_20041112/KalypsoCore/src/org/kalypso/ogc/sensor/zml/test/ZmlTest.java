package org.kalypso.ogc.sensor.zml.test;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import junit.framework.TestCase;

import org.kalypso.java.util.DoubleComparator;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

/**
 * @author schlienger
 */
public class ZmlTest extends TestCase
{
  private IObservation m_obs;
  private String m_obsID;
  private SimpleDateFormat df;
  private DoubleComparator dc;

  protected void setUp() throws Exception
  {
    super.setUp();

    final File zmlFile = new File( "./etc/schemas/zml/beispiel.zml" );
    assertTrue( zmlFile.exists() );

    m_obsID = zmlFile.getAbsolutePath();
    
    m_obs = ZmlFactory.parseXML( zmlFile.toURL(), m_obsID );
    
    df = new SimpleDateFormat( "dd.MM.yyyy" );
    dc = new DoubleComparator( 0 );
  }

  public void testGetName()
  {
    assertTrue( m_obs.getName().equals( "Eine Test-Observation" ) );
  }

  public void testIsEditable()
  {
    assertTrue( m_obs.isEditable() );
  }

  public void testGetIdentifier()
  {
    assertTrue( m_obs.getIdentifier().equals( m_obsID ) );
  }
  
  public void testGetTarget()
  {
    assertNull( m_obs.getTarget() );
  }

  public void testGetMetadataList()
  {
    final MetadataList mdl = m_obs.getMetadataList();
    assertNotNull( mdl );

    assertTrue( mdl.getProperty( "Pegelnullpunkt" ).equals( "10" ) );

    assertTrue( mdl.getProperty( "Alarmstufe 1" ).equals( "4.3" ) );
  }

  public void testGetAxisList()
  {
    final IAxis[] axes = m_obs.getAxisList();
    assertNotNull( axes );

    assertTrue( axes.length == 3 );

    int i = 2;
    assertTrue( axes[i].getName().equals( "Datum" ) );
    assertTrue( axes[i].getType().equals( "datum" ) );
    assertTrue( axes[i].getUnit().equals( "" ) );
    assertTrue( axes[i].getDataClass() == Date.class );
    assertTrue( axes[i].getPosition() == 0 );
    assertTrue( axes[i].isKey() );

    i = 1;
    assertTrue( axes[i].getName().equals( "Pegel1" ) );
    assertTrue( axes[i].getType().equals( "pegel" ) );
    assertTrue( axes[i].getUnit().equals( "m" ) );
    assertTrue( axes[i].getDataClass() == Double.class );
    assertTrue( axes[i].getPosition() == 1 );
    assertFalse( axes[i].isKey() );

    i = 0;
    assertTrue( axes[i].getName().equals( "Pegel2" ) );
    assertTrue( axes[i].getType().equals( "pegel" ) );
    assertTrue( axes[i].getUnit().equals( "m" ) );
    assertTrue( axes[i].getDataClass() == Double.class );
    assertTrue( axes[i].getPosition() == 2 );
    assertFalse( axes[i].isKey() );
  }

  public void testGetValues() throws SensorException, ParseException
  {
    final ITuppleModel values = m_obs.getValues( null );
    assertNotNull( values );
    
    assertEquals( values.getCount(), 21 );
    
    final IAxis[] axes = values.getAxisList();
    
    final IAxis dateAxis = ObservationUtilities.findAxisByName( axes, "Datum" );
    assertNotNull( dateAxis );
    
    final IAxis vAxis1 = ObservationUtilities.findAxisByName( axes, "Pegel1" );
    assertNotNull( vAxis1 );
    
    final IAxis vAxis2 = ObservationUtilities.findAxisByName( axes, "Pegel2" );
    assertNotNull( vAxis2 );
    
    assertEquals( values.getElement( 0, dateAxis ), df.parse( "01.01.2004" ) );
    assertTrue( dc.compare( values.getElement( 0, vAxis1 ), Double.valueOf( "1.0" ) ) == 0 );
    assertTrue( dc.compare( values.getElement( 0, vAxis2 ), new Double(11) ) == 0 );
    
    assertEquals( values.getElement( 20, dateAxis ), df.parse( "21.01.2004" ) );
    assertTrue( dc.compare( values.getElement( 20, vAxis1 ), Double.valueOf( "16.6" ) ) == 0 );
    assertTrue( dc.compare( values.getElement( 20, vAxis2 ), Double.valueOf( "18.5" ) ) == 0 );
  }

  public void testSetValues() throws SensorException, ParseException
  {
    final IAxis[] axes = m_obs.getAxisList();
    
    final IAxis dateAxis = ObservationUtilities.findAxisByName( axes, "Datum" );
    assertNotNull( dateAxis );
    
    final IAxis vAxis1 = ObservationUtilities.findAxisByName( axes, "Pegel1" );
    assertNotNull( vAxis1 );
    
    final IAxis vAxis2 = ObservationUtilities.findAxisByName( axes, "Pegel2" );
    assertNotNull( vAxis2 );
    
    final SimpleTuppleModel m = new SimpleTuppleModel( m_obs.getAxisList() );
    
    final Object[] t1 = new Object[3];
    t1[ dateAxis.getPosition() ] = df.parse( "20.01.2004" );
    t1[ vAxis1.getPosition() ] = new Double(44);
    t1[ vAxis2.getPosition() ] = new Double(11);
    m.addTupple( t1 );
    
    final Object[] t2 = new Object[3];
    t2[ dateAxis.getPosition() ] = df.parse( "21.01.2004" );
    t2[ vAxis1.getPosition() ] = new Double(55);
    t2[ vAxis2.getPosition() ] = new Double(22);
    m.addTupple( t2 );

    final Object[] t3 = new Object[3];
    t3[ dateAxis.getPosition() ] = df.parse( "22.01.2004" );
    t3[ vAxis1.getPosition() ] = new Double(66);
    t3[ vAxis2.getPosition() ] = new Double(33);
    m.addTupple( t3 );

    m_obs.setValues( m );
    
    final ITuppleModel values = m_obs.getValues( null );
    assertNotNull( values );
    
    assertEquals( values.getCount(), 22 );
    
    int i = 19;
    assertEquals( values.getElement( i, dateAxis ), df.parse( "20.01.2004" ) );
    assertTrue( dc.compare( values.getElement( i, vAxis1 ), Double.valueOf( "44" ) ) == 0 );
    assertTrue( dc.compare( values.getElement( i, vAxis2 ), Double.valueOf( "11" ) ) == 0 );

    i = 20;
    assertEquals( values.getElement( i, dateAxis ), df.parse( "21.01.2004" ) );
    assertTrue( dc.compare( values.getElement( i, vAxis1 ), Double.valueOf( "55" ) ) == 0 );
    assertTrue( dc.compare( values.getElement( i, vAxis2 ), Double.valueOf( "22" ) ) == 0 );
    
    i = 21;
    assertEquals( values.getElement( i, dateAxis ), df.parse( "22.01.2004" ) );
    assertTrue( dc.compare( values.getElement( i, vAxis1 ), Double.valueOf( "66" ) ) == 0 );
    assertTrue( dc.compare( values.getElement( i, vAxis2 ), Double.valueOf( "33" ) ) == 0 );
  }
}