package org.kalypso.kalypso1d2d.pjt.map;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.GMLNodeResult;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.wizards.results.ResultAddLayerCommandData;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

public class NodeThemeInfo implements IKalypsoThemeInfo
{
  private IKalypsoFeatureTheme m_theme;

  private QName m_actPropQname;

  private final double m_grabDistance = 50;

  private IFEDiscretisationModel1d2d m_discretisationModel = null;

  private CommandableWorkspace m_workspace = null;

  private FeatureList m_featureList = null;

  private String m_propertyLabel;

  @Override
  public void init( final IKalypsoTheme theme, final Properties props )
  {
    Assert.isLegal( theme instanceof IKalypsoFeatureTheme );

    m_theme = (IKalypsoFeatureTheme)theme;

    m_workspace = m_theme.getWorkspace();

    m_featureList = m_theme.getFeatureList();

    m_actPropQname = determinePropertyType( theme );

    m_propertyLabel = NodeResultHelper.translateNodeParameterType( m_actPropQname );

    // FIXME: does not work correctly for external themes -> before fixing, ask Ilya what exactly this code is supposed to do
    try
    {
      final IScenarioDataProvider caseDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
      m_discretisationModel = caseDataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  private QName determinePropertyType( final IKalypsoTheme theme )
  {
    final String propertyNameFromTheme = getPropertyNameFromTheme( theme );

    if( NodeResultHelper.VELO_TYPE.toLowerCase().equals( propertyNameFromTheme ) )
      return new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, NodeResultHelper.VELOCITY );

    if( StringUtils.isBlank( propertyNameFromTheme ) )
      return null;

    return new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, propertyNameFromTheme.toLowerCase() );
  }

  private String getPropertyNameFromTheme( final IKalypsoTheme theme )
  {
    /* the theme has a property with the property name */
    final String propertyName = theme.getProperty( ResultAddLayerCommandData.PROPERTY_RESULT_NODE_PARAMETER_TYPE, null );
    if( !StringUtils.isBlank( propertyName ) )
      return propertyName;

    /* backwards compatibility: formerly the property name was parsed from the theme name :-( */
    // TODO: should be removed in future versions
    final StringTokenizer lTokenizer = new StringTokenizer( theme.getLabel(), "," ); //$NON-NLS-1$
    if( lTokenizer.countTokens() > 2 )
    {
      lTokenizer.nextToken();
      return lTokenizer.nextToken().trim();
    }

    return null; //$NON-NLS-1$
  }

  @Override
  public void appendInfo( final Formatter formatter, final GM_Position pos )
  {
    // not yet implemented, use quick-info
    appendQuickInfo( formatter, pos );
  }

  @SuppressWarnings( "unchecked" )
  @Override
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_theme );

    if( m_featureList == null )
      return;

    if( m_discretisationModel == null )
      return;

    if( m_actPropQname == null )
      return;

    final Object nodeObject = m_discretisationModel.find2DElement( GeometryFactory.createGM_Point( pos, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() ), 0.01 );
    if( nodeObject == null )
      return;

    /* Search for the first feature which provides a value */
    final Object value = getInterpolatedValue( nodeObject, pos );

    if( value instanceof Double && !Double.isNaN( (Double)value ) )
    {
      formatter.format( "%s: %.3f", m_propertyLabel, value ); //$NON-NLS-1$
      return;
    }
    else
    {
      if( value != null && value instanceof List< ? > )
      {
        final List<Double> vector = (List<Double>)value;
        if( vector.size() != 2 )
          return;

        final double vx = vector.get( 0 );
        final double vy = vector.get( 1 );

        formatter.format( "%s: %.2fm/s, %.2f°", m_propertyLabel, Math.sqrt( vx * vx + vy * vy ), GeometryUtilities.directionFromVector( vx, vy ) ); //$NON-NLS-1$
      }
    }
  }

  // FIXME: chaos code; needs heavy cleanup...
  private Object getInterpolatedValue( final Object nodeObject, final GM_Position pos )
  {
    if( nodeObject instanceof IPolyElement )
    {
      final IPolyElement lPolyEl = (IPolyElement)nodeObject;

      final IFE1D2DNode[] nodes = lPolyEl.getNodes();
      if( nodes.length > 5 )
      {
        return getNodePropertyAtPos( pos );
      }
      final String coordinateSystem = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      final List<GM_Position> lListPositionWithValues = new ArrayList<>();
      final List<GM_Position> lListPositionWithValues2 = new ArrayList<>();
      for( final IFE1D2DNode actNode : nodes )
      {
        final Feature nodeRes = GeometryUtilities.findNearestFeature( GeometryFactory.createGM_Point( actNode.getPoint().getPosition(), coordinateSystem ), m_grabDistance, m_featureList, GMLNodeResult.QNAME_PROP_LOCATION );
        if( nodeRes == null )
          continue;

        Object value = nodeRes.getProperty( m_actPropQname );
        if( value == null )
          continue;

        if( NodeResultHelper.WAVE_DIRECTION_TYPE.equals( m_actPropQname.getLocalPart() ) )
        {
          final List<Double> vector = new ArrayList<>();
          vector.add( Math.cos( (Double)value * (2 * Math.PI) / 360 ) );
          vector.add( Math.sin( (Double)value * (2 * Math.PI) / 360 ) );
          value = vector;
        }

        final INodeResult nodeResAdapter = (INodeResult)nodeRes.getAdapter( INodeResult.class );
        if( value instanceof Double )
        {
          lListPositionWithValues.add( GeometryFactory.createGM_Position( nodeResAdapter.getPoint().getX(), nodeResAdapter.getPoint().getY(), (Double)value ) );
        }
        else if( value instanceof List< ? > )
        {
          final List< ? > vector = (List< ? >)value;
          if( vector.size() != 2 )
            continue;

          lListPositionWithValues.add( GeometryFactory.createGM_Position( nodeResAdapter.getPoint().getX(), nodeResAdapter.getPoint().getY(), (Double)vector.get( 0 ) ) );
          lListPositionWithValues2.add( GeometryFactory.createGM_Position( nodeResAdapter.getPoint().getX(), nodeResAdapter.getPoint().getY(), (Double)vector.get( 1 ) ) );
        }
      }
      if( lListPositionWithValues.size() < 3 )
      {
        return getNodePropertyAtPos( pos );
      }
      GM_Triangle lTri = GeometryFactory.createGM_Triangle( lListPositionWithValues.get( 0 ), lListPositionWithValues.get( 1 ), lListPositionWithValues.get( 2 ), coordinateSystem );
      GM_Triangle lTri2 = null;
      if( lListPositionWithValues2.size() > 2 )
        lTri2 = GeometryFactory.createGM_Triangle( lListPositionWithValues2.get( 0 ), lListPositionWithValues2.get( 1 ), lListPositionWithValues2.get( 2 ), coordinateSystem );
      final Object lRes = getValueFromTrianglesAtPosition( pos, lTri, lTri2 );
      if( lRes != null )
      {
        return lRes;
      }
      else if( lListPositionWithValues.size() > 4 )
      {
        lTri = GeometryFactory.createGM_Triangle( lListPositionWithValues.get( 0 ), lListPositionWithValues.get( 2 ), lListPositionWithValues.get( 3 ), coordinateSystem );
        if( lListPositionWithValues2.size() > 2 )
          lTri2 = GeometryFactory.createGM_Triangle( lListPositionWithValues2.get( 0 ), lListPositionWithValues2.get( 2 ), lListPositionWithValues2.get( 3 ), coordinateSystem );
        return getValueFromTrianglesAtPosition( pos, lTri, lTri2 );
      }
      else
      {
        return getNodePropertyAtPos( pos );
      }

    }
    else
    {
      return getNodePropertyAtPos( pos );
    }
  }

  // TODO: switch to this implementation
  // private Pair<Double, Double> getInterpolatedPair( final GM_Point pPointToInterpolateAt, final Map<GM_Point,
  // Pair<Double, Double>> pMapPointsValues )
  // {
  // GM_Triangle lTriFirst = null;
  // GM_Triangle lTriSecond = null;
  // try
  // {
  // final List<GM_Point> lListAllPoints = new ArrayList<GM_Point>();
  // final List<Double> lListAllFirsts = new ArrayList<Double>();
  // final List<Double> lListAllSeconds = new ArrayList<Double>();
  //
  // final Set<GM_Point> lSetPoints = pMapPointsValues.keySet();
  // for( final GM_Point gmPoint : lSetPoints )
  // {
  // lListAllPoints.add( gmPoint );
  // lListAllFirsts.add( pMapPointsValues.get( gmPoint ).first );
  // lListAllSeconds.add( pMapPointsValues.get( gmPoint ).second );
  // }
  // Map<GM_Point, Double> lMapFirst = CollectionsHelper.joinListsToMap( lListAllPoints.subList( 0, 3 ),
  // lListAllFirsts.subList( 0, 3 ) );
  // lTriFirst = GeometryUtilities.createTriangleForBilinearInterpolation( lMapFirst );
  // if( lTriFirst.contains( pPointToInterpolateAt ) )
  // {
  // final Map<GM_Point, Double> lMapSecond = CollectionsHelper.joinListsToMap( lListAllPoints.subList( 0, 3 ),
  // lListAllFirsts.subList( 0, 3 ) );
  // lTriSecond = GeometryUtilities.createTriangleForBilinearInterpolation( lMapSecond );
  // return new Pair<Double, Double>( lTriFirst.getValue( pPointToInterpolateAt.getPosition() ), lTriSecond.getValue(
  // pPointToInterpolateAt.getPosition() ) );
  // }
  // else
  // {
  // lListAllPoints.remove( 1 );
  // lListAllFirsts.remove( 1 );
  // lListAllSeconds.remove( 1 );
  // lMapFirst = CollectionsHelper.joinListsToMap( lListAllPoints, lListAllFirsts );
  // lTriFirst = GeometryUtilities.createTriangleForBilinearInterpolation( lMapFirst );
  // final Map<GM_Point, Double> lMapSecond = CollectionsHelper.joinListsToMap( lListAllPoints, lListAllFirsts );
  // lTriSecond = GeometryUtilities.createTriangleForBilinearInterpolation( lMapSecond );
  // return new Pair<Double, Double>( lTriFirst.getValue( pPointToInterpolateAt.getPosition() ), lTriSecond.getValue(
  // pPointToInterpolateAt.getPosition() ) );
  // }
  // }
  // catch( final Exception e )
  // {
  // return null;
  // }
  // }

  private Object getValueFromTrianglesAtPosition( final GM_Position pos, final GM_Triangle lTri, final GM_Triangle lTri2 )
  {
    if( lTri == null || !lTri.contains( pos ) )
      return null;

    if( lTri2 == null )
      return lTri.getValue( pos );

    final List<Double> lListRes = new ArrayList<>();
    lListRes.add( lTri.getValue( pos ) );
    lListRes.add( lTri2.getValue( pos ) );

    if( NodeResultHelper.WAVE_DIRECTION_TYPE.equals( m_actPropQname.getLocalPart() ) )
      return GeometryUtilities.directionFromVector( lListRes.get( 0 ), lListRes.get( 1 ) );

    return lListRes;
  }

  private Object getNodePropertyAtPos( final GM_Position pos )
  {
    final Feature nodeRes = GeometryUtilities.findNearestFeature( GeometryFactory.createGM_Point( pos, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() ), m_grabDistance, m_featureList, GMLNodeResult.QNAME_PROP_LOCATION );
    final Feature feature = FeatureHelper.getFeature( m_workspace, nodeRes );
    return feature == null ? null : feature.getProperty( m_actPropQname );
  }
}