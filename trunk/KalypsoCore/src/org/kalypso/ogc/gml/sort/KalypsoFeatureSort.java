package org.kalypso.ogc.gml.sort;

import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.feature.event.ModellEventProvider;
import org.deegree.model.feature.event.ModellEventProviderAdapter;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.sort.JMSpatialIndex;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.feature.FeatureFactory;
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

  public KalypsoFeatureSort( final CS_CoordinateSystem crs )
  {
    myStyles = new ArrayList();
    myStylesHash = new Hashtable();
    myIsDirty = true;
    myCrs = crs;
    mySort = JMSpatialIndexFactory.createSpatialIndex();
    myFeatures = new ArrayList();
  }

  public CS_CoordinateSystem getCoordinatesSystem()
  {
    return myCrs;
  }

  public void setCoordinatesSystem( CS_CoordinateSystem crs )
  {
    if( !myCrs.equals( crs ) )
      throw new UnsupportedOperationException();
  }

  public void modifiedFeature( Feature feature )
  {
    styleFeature( feature, getStyles() );
    fireModellEvent( null );
  }

  public void modifiedFeatures( Feature[] feature )
  {
    UserStyle[] styles = getStyles();
    for( int i = 0; i < feature.length; i++ )
      styleFeature( feature[i], styles );
    fireModellEvent( null );
  }

  public void add( Feature fe ) throws Exception
  {
    transformFeature( fe );
    styleFeature( fe, getStyles() );
    mySort.add( fe );
    myFeatures.add( fe );
  }

  public void remove( Feature fe )
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

  public void paintSelected( final Graphics g, final GeoTransform projection, final double scale,
      final GM_Envelope boundingBox, final int selectionId )
  {
    if( myIsDirty )
      reStyleAll();
    List list = new ArrayList();
    mySort.query( boundingBox, list );
    
    for( int i = 0; i < myStyles.size(); i++ )
    {
      // int styleNo = getStyleNo( style );
      for( final Iterator it = list.iterator(); it.hasNext(); )
      {
        final Feature kalypsoFeature = (Feature)it.next();
        if( selectionId == -1 || kalypsoFeature.isSelected( selectionId ) )
          kalypsoFeature.paint( g, projection, i, scale );
      }
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
          // TODO: andreas: sollte dieses catch nicht einfach wegfallen, damit die Exception
          // weiter propagiert wird?
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

  private void styleFeature( Feature feature, UserStyle[] styles )
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
      Feature fe = (Feature)list.get( i );
      fe.setDisplayElements( styles );
    }
    myIsDirty = false;
  }

  public UserStyle[] getStyles()
  {
    return (UserStyle[])myStyles.toArray( new UserStyle[myStyles.size()] );
  }

//  private int getStyleNo( UserStyle style )
//  {
//    return myStyles.indexOf( style );
//  }

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