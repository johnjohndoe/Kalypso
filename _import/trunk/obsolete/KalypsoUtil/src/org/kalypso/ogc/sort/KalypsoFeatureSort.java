package org.kalypso.ogc.sort;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.sort.JMSpatialIndex;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.event.ModellEventProvider;
import org.kalypso.ogc.event.ModellEventProviderAdapter;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author vdoemming
 */
public class KalypsoFeatureSort implements ModellEventListener, ModellEventProvider
{
  private final ModellEventProviderAdapter myModellEventProviderAdapter = new ModellEventProviderAdapter();

  private CS_CoordinateSystem myCrs = null;

  private List myStyles = new ArrayList();

  private Hashtable myStylesHash = null; // style,StyleContext

  private JMSpatialIndex mySort = null;

  private boolean myIsDirty = true;

  private List myFeatures = null;

  private static final String DEFAULT_STYLE = "default";

  public KalypsoFeatureSort( CS_CoordinateSystem crs )
  {
    init( crs, null );
  }

  public KalypsoFeatureSort( CS_CoordinateSystem crs, GM_Envelope boundingBox )
  {
    init( crs, boundingBox );
  }

  private void init( CS_CoordinateSystem crs, GM_Envelope boundingBox )
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
      throw new UnsupportedOperationException();
  }

  public void modifiedFeature( KalypsoFeature feature )
  {
    styleFeature( feature, getStyles() );
    fireModellEvent( null );
  }

  public void modifiedFeatures( KalypsoFeature[] feature )
  {
    UserStyle[] styles = getStyles();
    for( int i = 0; i < feature.length; i++ )
      styleFeature( feature[i], styles );
    fireModellEvent( null );
  }

  public void add( KalypsoFeature fe ) throws Exception
  {
    transformFeature( fe );
    styleFeature( fe, getStyles() );
    mySort.add( fe );
    myFeatures.add( fe );
  }

  public void remove( KalypsoFeature fe )
  {
    mySort.remove( fe );
    myFeatures.remove( fe );
  }

  public List getAllFeatures()
  {
    return myFeatures;
  }

  public List queryAll( List list )
  {
    return mySort.queryAll( list );
  }

  public List query( GM_Envelope env, List list )
  {
    return mySort.query( env, list );
  }

  public List query( GM_Position position, List list )
  {
    return mySort.query( position, list );
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
      ( (KalypsoFeature)it.next() ).paint( g, projection, styleNo, scale );
    }
  }

  public void paintSelected( Graphics g, GeoTransform projection, UserStyle style, double scale,
      GM_Envelope boundingBox, int selectionId )
  {
    if( myIsDirty )
      reStyleAll();
    List list = new ArrayList();
    mySort.query( boundingBox, list );
    Iterator it = list.iterator();
    int styleNo = getStyleNo( style );
    while( it.hasNext() )
    {
      KalypsoFeature kalypsoFeature = (KalypsoFeature)it.next();
      if( kalypsoFeature.isSelected( selectionId ) )
        kalypsoFeature.paint( g, projection, styleNo, scale );
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
      ( (KalypsoFeature)it.next() ).paint( g, projection, styleNo );
    }
  }

  private void transformFeature( KalypsoFeature fe ) throws Exception
  {
    GeoTransformer transformer = new GeoTransformer( myCrs );

    FeatureTypeProperty ftp[] = fe.getFeatureType().getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      Object prop = fe.getProperty( ftp[i].getName() );
      if( prop != null && prop instanceof KalypsoFeature )
        transformFeature( (KalypsoFeature)prop );
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
  // SLDs freigeben
  }

  public void addStyle( KalypsoUserStyle style )
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
      {
        myStylesHash.put( style, new StyleContext( style ) );
        style.addModellListener( this );
      }
      myStyles.add( style );
      setDirty();
    }
  }

  public void removeStyle( KalypsoUserStyle style )
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
      {
        myStylesHash.remove( style );
        style.removeModellListener( this );
      }
      setDirty();
    }
  }

  private void styleFeature( KalypsoFeature feature, UserStyle[] styles )
  {
    feature.setDisplayElements( styles );
  }

  private void reStyleAll()
  {
    List list = new ArrayList();
    mySort.queryAll( list );
    UserStyle[] styles = getStyles();
    for( int i = 0; i < list.size(); i++ )
    {
      KalypsoFeature fe = (KalypsoFeature)list.get( i );
      fe.setDisplayElements( styles );
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

  /**
   * @param listener
   */
  public void addModellListener( ModellEventListener listener )
  {
    myModellEventProviderAdapter.addModellListener( listener );
  }

  /**
   * @param event
   */
  public void fireModellEvent( ModellEvent event )
  {
    myModellEventProviderAdapter.fireModellEvent( event );
  }

  /**
   * @param listener
   */
  public void removeModellListener( ModellEventListener listener )
  {
    myModellEventProviderAdapter.removeModellListener( listener );
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

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( ModellEvent modellEvent )
  {
    {
      if( modellEvent.getType() == ModellEvent.STYLE_CHANGE )
        reStyleAll(); // TODO nur den geaenderten style neu rendern
      fireModellEvent( modellEvent );
    }

  }
}