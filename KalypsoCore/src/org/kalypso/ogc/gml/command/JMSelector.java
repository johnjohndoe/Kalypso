package org.kalypso.ogc.gml.command;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author von Dömming
 */
public class JMSelector
{
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

  public List perform( final List listFe, final int selectionId )
  {
    final List result = new ArrayList(); // alle veraenderten fe
    final Iterator iterator = listFe.iterator();
    while( iterator.hasNext() )
    {
      final Feature fe = (Feature)iterator.next();

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

  /**
   * // selects all features (display elements) that are located within the
   * submitted bounding box. // GMLGeometry
   * gmlGeometry=GMLFactory.createGMLGeometry(bbox);
   * 
   * //Operation operation=new
   * SpatialOperation(OperationDefines.WITHIN,myPropertyName,gmlGeometry);
   * //Filter filter=new ComplexFilter(operation);
   */
  public List select( final GM_Envelope env, final FeatureList list,
      final boolean selectWithinBoxStatus, final int selectionId )
  {
    try
    {
      final List testFE = new ArrayList();
      
      final List features = list.query( env, new ArrayList() );
      final Iterator containerIterator = features.iterator();

      while( containerIterator.hasNext() )
      {
        final Feature fe = (Feature)containerIterator.next();

        final GM_Object defaultGeometryProperty = fe.getDefaultGeometryProperty();
        final CS_CoordinateSystem coordinateSystem = defaultGeometryProperty.getCoordinateSystem();
        
      final GM_Surface bbox = GeometryFactory.createGM_Surface( env, coordinateSystem );

        if( ( selectWithinBoxStatus && bbox.contains( defaultGeometryProperty ) )
            || ( !selectWithinBoxStatus && bbox.intersects( defaultGeometryProperty ) ) )
          testFE.add( fe );
      }

      return perform( testFE, selectionId );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    return new ArrayList();
  }

  /**
   * selects all features that intersects the submitted point
   */
  public List select( final GM_Position position, final FeatureList list, int selectionId )
  {
    final List resultList = new ArrayList();
    final List testFe = new ArrayList();
    
    final List features = list.query( position, new ArrayList() );
    for( final Iterator containerIterator = features.iterator(); containerIterator.hasNext(); )
    {
      final Feature feature = (Feature)containerIterator.next();

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
        resultList.addAll( select( position, 0.0001d, list, false, selectionId ) );
      }
    }

    resultList.addAll( perform( testFe, selectionId ) );

    return resultList;
  }

  /**
   * selects all features (display elements) that are located within the circle
   * described by the position and the radius.
   */
  public List select( GM_Position pos, double r, final FeatureList list,
      boolean withinStatus, int selectionId )
  {
    final List resultDE = select( GeometryFactory.createGM_Envelope( pos.getX() - r,
        pos.getY() - r, pos.getX() + r, pos.getY() + r ), list, withinStatus, selectionId );

    return resultDE;
  }

  public Feature selectNearest( final GM_Position pos, final double r,
      final FeatureList list, final boolean withinStatus, final int selectionId )
  {
    Feature result = null;
    double dist = 0;
    final List listFE = select( pos, r, list, withinStatus, selectionId );
    for( int i = 0; i < listFE.size(); i++ )
    {
      final Feature fe = (Feature)listFE.get( i );
      if( result == null
          || result.getDefaultGeometryProperty().distance( fe.getDefaultGeometryProperty() ) < dist )
        result = fe;
    }
    return result;
  }
}