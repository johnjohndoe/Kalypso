package org.deegree_impl.model.sort;

import java.awt.Graphics;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.sort.JMSpatialIndex;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.feature.FeatureFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class DisplayContextSort
{
  private CS_CoordinateSystem myCrs = null;

  private List myStyles = new ArrayList();

  private Hashtable myStylesHash = null; // style,StyleContext

  private JMSpatialIndex mySort = null;

  private boolean myIsDirty = true;

  private List myFeatures = null;

  private static final String DEFAULT_STYLE = "default";

  public DisplayContextSort( CS_CoordinateSystem crs )
  {
    init( crs, null );
  }

  public DisplayContextSort( CS_CoordinateSystem crs, GM_Envelope boundingBox )
  {
    init( crs, boundingBox );
  }

  public void init( CS_CoordinateSystem crs, GM_Envelope boundingBox )
  {
    myStyles = new ArrayList();
    myStylesHash = new Hashtable();
    myIsDirty = true;
    myCrs = crs;
    mySort = JMSpatialIndexFactory.createSpatialIndex( boundingBox );
    myFeatures = new ArrayList();
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myCrs;
  }

  public void setCoordinatesSystem( CS_CoordinateSystem crs ) throws Exception
  {
    if( !myCrs.equals( crs ) )
      throw new Exception( "not supported" );
  }

  public DisplayContext add( Feature fe ) throws Exception
  {
    transformFeature( fe );
    DisplayContext dc = new DisplayContext( fe, getStyles() );
    mySort.add( dc );
    myFeatures.add( fe );
    return dc;
  }

  public void remove( Feature fe )
  {
    remove( new DisplayContext( fe ) );
  }

  public void remove( DisplayContext dc )
  {
    mySort.remove( dc );
    myFeatures.remove( dc.getFeature() );
  }

  public List getAllFeatures()
  {
    return myFeatures;
  }

  public void paint( Graphics g, GeoTransform projection, UserStyle style, double scale,
      GM_Envelope boundingBox )
  {
    if( myIsDirty )
      reStyleAll();
    List list = new ArrayList();
    mySort.query( boundingBox, list );
    Iterator it = list.iterator();
    int styleNo = getStyleNo( style );
    while( it.hasNext() )
    {
      ( (DisplayContext)it.next() ).paint( g, projection, styleNo, scale );
    }
  }

  public void paint( Graphics g, GeoTransform projection, UserStyle style, GM_Envelope boundingBox )
  {
    if( myIsDirty )
      reStyleAll();
    List list = new ArrayList();
    mySort.query( boundingBox, list );
    Iterator it = list.iterator();
    int styleNo = getStyleNo( style );
    while( it.hasNext() )
    {
      ( (DisplayContext)it.next() ).paint( g, projection, styleNo );
    }
  }

  private void transformFeature( Feature fe ) throws Exception
  {
    GeoTransformer transformer = new GeoTransformer( myCrs );

    FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof Feature )
        transformFeature( (Feature)prop );
      else if( prop != null && prop instanceof GM_Object )
      {
        try
        {
          if( !( (GM_Object)prop ).getCoordinateSystem().equals( myCrs ) )
          {
            GM_Object newGeo = transformer.transform( (GM_Object)prop );
            FeatureProperty fProp = FeatureFactory.createFeatureProperty( ftp[i].getName(), newGeo );
            fe.setProperty( fProp );
          }
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  public void dispose()
  {
  // SLD freigeben
  }

  public void addStyle( UserStyle style )
  {

    if( myStyles.contains( style ) )
    {
      if( style == null )
        ( (StyleContext)myStylesHash.get( DEFAULT_STYLE ) ).inc();
      else
        ( (StyleContext)myStylesHash.get( style ) ).inc();
    }
    else
    {
      if( style == null )
        myStylesHash.put( DEFAULT_STYLE, new StyleContext( style ) );
      else
        myStylesHash.put( style, new StyleContext( style ) );
      myStyles.add( style );
      setDirty();
    }
  }

  public void removeStyle( UserStyle style )
  {
    StyleContext sc = null;
    if( style == null )
      sc = (StyleContext)myStylesHash.get( DEFAULT_STYLE );
    else
      sc = (StyleContext)myStylesHash.get( style );
    sc.dec();
    if( sc.counter == 0 )
    {
      myStyles.remove( style );
      if( style == null )
        myStylesHash.remove( DEFAULT_STYLE );
      else
        myStylesHash.remove( style );
      setDirty();

    }
  }

  private void reStyleAll()
  {
    List list = new ArrayList();
    mySort.queryAll( list );
    UserStyle[] styles = getStyles();
    for( int i = 0; i < list.size(); i++ )
    {
      DisplayContext dc = (DisplayContext)list.get( i );
      dc.updateDisplayElements( styles );
    }
    myIsDirty = false;
  }

  private UserStyle[] getStyles()
  {
    return (UserStyle[])myStyles.toArray( new UserStyle[myStyles.size()] );
  }

  private int getStyleNo( UserStyle style )
  {
    return myStyles.indexOf( style );
  }

  private void setDirty()
  {
    myIsDirty = true;
  }

  public GM_Envelope getBoundingBox()
  {
    return mySort.getBoundingBox();
  }

  private class StyleContext
  {
    public UserStyle myStyle = null;

    public int counter = 1;

    public StyleContext( UserStyle style )
    {
      myStyle = style;
    }

    public void inc()
    {
      counter++;
    }

    public void dec()
    {
      counter--;
    }
  }
}