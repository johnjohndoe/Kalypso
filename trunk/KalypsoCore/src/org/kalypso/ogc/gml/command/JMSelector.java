package org.kalypso.ogc.gml.command;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;

/**
 * DOCUMENT ME!
 * 
 * @author $author$
 */
public class JMSelector //implements JMThemeListener
{
  //selectionMode:
  public final static String[] SELECTION_MODE_TEXT =
  { "toggle", "select", "unselect" };

  public final static int MODE_TOGGLE = 1;

  public final static int MODE_SELECT = 2;

  public final static int MODE_UNSELECT = 3;

  public final static int MODE_COLLECT = 4;

  private int mySelectionMode = MODE_TOGGLE;

  public JMSelector()
  {
  //
  }

  public JMSelector( int selectionMode )
  {
    mySelectionMode = selectionMode;
  }

  public void setSelectionMode( int selectionMode )
  {
    this.mySelectionMode = selectionMode;
  }

  public List perform( List listFe, int selectionId )
  {
    List result = new ArrayList(); // alle veraenderten fe
    Iterator iterator = listFe.iterator();
    while( iterator.hasNext() )
    {
      KalypsoFeature fe = (KalypsoFeature)iterator.next();

      switch( mySelectionMode )
      {
      case MODE_TOGGLE:
        result.add( fe );
        break;
      case MODE_SELECT:
        if( !fe.isSelected( selectionId ) )
          result.add( fe );
        break;
      case MODE_UNSELECT:
        if( fe.isSelected( selectionId ) )
          result.add( fe );
        break;
      case MODE_COLLECT:
        return listFe;

      default:
        break;
      }
    }
    return result;

  }

  /*
   * // selects all features (display elements) that are located within the
   * submitted bounding box. // GMLGeometry
   * gmlGeometry=GMLFactory.createGMLGeometry(bbox);
   * 
   * //Operation operation=new
   * SpatialOperation(OperationDefines.WITHIN,myPropertyName,gmlGeometry);
   * //Filter filter=new ComplexFilter(operation);
   */
  public List select( GM_Envelope env, final IKalypsoTheme theme, boolean selectWithinBoxStatus,
      int selectionId )
  {
    List resultList = new ArrayList();
    IKalypsoLayer layer = theme.getLayer();
    if( !( layer instanceof KalypsoFeatureLayer ) )
      return resultList;
    try
    {
      List testFE = new ArrayList();
      GM_Surface bbox = GeometryFactory.createGM_Surface( env, layer.getCoordinatesSystem() );
      List features = ( (KalypsoFeatureLayer)layer ).getSort().query( env, new ArrayList() );
      Iterator containerIterator = features.iterator();

      while( containerIterator.hasNext() )
      {
        KalypsoFeature fe = (KalypsoFeature)containerIterator.next();

        if( ( selectWithinBoxStatus && bbox.contains( fe.getDefaultGeometryProperty() ) )
            || ( !selectWithinBoxStatus && bbox.intersects( fe.getDefaultGeometryProperty() ) ) )
          testFE.add( fe );
      }

      resultList = perform( testFE, selectionId );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return resultList;
  }

  // selects all features that intersects the submitted
  // point.
  public List select( GM_Position position, final IKalypsoTheme theme, int selectionId )
  {
    List resultList = new ArrayList();
    IKalypsoLayer layer = theme.getLayer();
    if( !( layer instanceof KalypsoFeatureLayer ) )
      return resultList;
    List testFe = new ArrayList();
    List features = ( (KalypsoFeatureLayer)layer ).getSort().query( position, new ArrayList() );

    Iterator containerIterator = features.iterator();

    while( containerIterator.hasNext() )
    {
      KalypsoFeature feature = (KalypsoFeature)containerIterator.next();

      try
      {
        if( feature.getDefaultGeometryProperty().contains( position ) )
          testFe.add( feature );
      }
      catch( Exception err )
      {
        System.out.println( err.getMessage() );
        System.out.println( "...using workaround \"box selection\"" );
        System.out.println( "set view dependent radius" );
        resultList.addAll( select( position, 0.0001d, theme, false, selectionId ) );
      }
    }

    resultList.addAll( perform( testFe, selectionId ) );

    return resultList;
  }

  //           selects all features (display elements) that are located within the circle
  // described by the position and the radius.
  public List select( GM_Position pos, double r, final IKalypsoTheme theme, boolean withinStatus,
      int selectionId )
  {
    List resultDE = select( GeometryFactory.createGM_Envelope( pos.getX() - r, pos.getY() - r, pos
        .getX()
        + r, pos.getY() + r ), theme, withinStatus, selectionId );

    return resultDE;
  }

  public KalypsoFeature selectNearest( GM_Position pos, double r, final IKalypsoTheme theme,
      boolean withinStatus, int selectionId )
  {
    KalypsoFeature result = null;
    double dist = 0;
    List listFE = select( pos, r, theme, withinStatus, selectionId );
    for( int i = 0; i < listFE.size(); i++ )
    {
      KalypsoFeature fe = (KalypsoFeature)listFE.get( i );
      if( result == null
          || result.getDefaultGeometryProperty().distance( fe.getDefaultGeometryProperty() ) < dist )
        result = fe;
    }
    return result;
  }
}