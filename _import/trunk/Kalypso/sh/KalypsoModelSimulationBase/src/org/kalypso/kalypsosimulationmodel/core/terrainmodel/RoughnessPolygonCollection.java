package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * The default implementation of {@link IRoughnessPolygonCollection} based on {@link FeatureWrapperCollection}
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Patrice Congo
 */
public class RoughnessPolygonCollection extends FeatureWrapperCollection<IRoughnessPolygon> implements IRoughnessPolygonCollection
{

  public RoughnessPolygonCollection( Feature featureCol, Class<IRoughnessPolygon> fwClass, QName featureMemberProp )
  {
    super( featureCol, fwClass, featureMemberProp );
  }

  public RoughnessPolygonCollection( Feature parentFeature, QName childQName, QName featureMemberProp, Class<IRoughnessPolygon> fwClass ) throws IllegalArgumentException
  {
    super( parentFeature, childQName, featureMemberProp, fwClass );
  }

  public List<IRoughnessPolygon[]> checksOverlapping( )
  {

    return null;
  }

  public IRoughnessEstimateSpec getRoughnessEstimateSpec( GM_Polygon polygon )
  {
    return null;
  }

  @SuppressWarnings("unchecked")
  public List<IRoughnessPolygon> getRoughnessPolygons( )
  {
    Object object = getWrappedFeature().getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON );
    return (List<IRoughnessPolygon>) getWrappedFeature().getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_ROUGHNESS_LAYER_POLYGON );
  }

  @SuppressWarnings("unchecked")
  public IRoughnessPolygon[] select( GM_Point point )
  {
    GM_Position position = GeometryFactory.createGM_Position( point.getX(), point.getY() );
    List<Feature> selectedPolygonsList = getWrappedList().query( position, null );
    IRoughnessPolygon[] dstPolygonsArray = new IRoughnessPolygon[selectedPolygonsList.size()];
    Iterator<Feature> iterator = selectedPolygonsList.listIterator();
    int i = 0;
    while(iterator.hasNext()){
      dstPolygonsArray[i++] = new RoughnessPolygon(iterator.next());
    }
    return dstPolygonsArray;
  }

}
