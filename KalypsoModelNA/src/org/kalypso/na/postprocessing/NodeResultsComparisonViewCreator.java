package org.kalypso.na.postprocessing;

import java.awt.Color;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.template.obsdiagview.Obsdiagview;
import org.kalypso.template.obsdiagview.TypeAxis;
import org.kalypso.template.obsdiagview.TypeAxisMapping;
import org.kalypso.template.obsdiagview.TypeCurve;
import org.kalypso.template.obsdiagview.TypeDirection;
import org.kalypso.template.obsdiagview.TypeObservation;
import org.kalypso.template.obsdiagview.TypePosition;
import org.kalypso.template.obsdiagview.Obsdiagview.Legend;
import org.kalypso.template.obstableview.Obstableview;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeRenderingRule;
import org.kalypso.template.obstableview.Obstableview.Rules;

public final class NodeResultsComparisonViewCreator
{
  final static Color[] OBS_COLOR = new Color[] { new Color( 78, 227, 28 ), new Color( 0, 128, 64 ) };

  public final static Obsdiagview createView( final String title, final String legendTitle, final String izPath, final String calculatePath, final String nodeID )
  {
    final Obsdiagview view = new Obsdiagview();
    view.setTitle( title );
    final Legend legend = new Legend();
    view.setLegend( legend );
    legend.setTitle( legendTitle );
    legend.setVisible( true );
    view.setFeatures( "Vorhersage;Alarmstufen" );
    final List<TypeAxis> axis = view.getAxis();
    axis.add( createDateAxis() );
    axis.add( createDischargeAxis() );
    final Map<String, String> mappingsMap = new HashMap<String, String>();
    mappingsMap.put( "date", "Datum" );
    mappingsMap.put( "Q", "Abfluss" );
    final List<TypeObservation> observationList = view.getObservation();
    observationList.add( createDiagObservation( izPath, "Q - " + nodeID + " - Ist Zustand", "C1", OBS_COLOR[0], mappingsMap ) );
    observationList.add( createDiagObservation( calculatePath, "Q - " + nodeID + " - Szenario", "C2", OBS_COLOR[1], mappingsMap ) );
    return view;
  }

  public final static Obstableview createTableView( final String izPath, final String calculatedPath )
  {
    final Obstableview view = new Obstableview();
    view.setFeatures( "Vorhersage;Alarmstufen" );
    final List<org.kalypso.template.obstableview.TypeObservation> observationList = view.getObservation();
    observationList.add( createTableObservation( izPath, "Ist Zustand", "C1", "Abfluss" ) );
    observationList.add( createTableObservation( calculatedPath, "Szenario", "C2", "Abfluss" ) );
    view.setAlphaSort( false );
    final TypeRenderingRule rule1 = new TypeRenderingRule();
    rule1.setMask( 16 );
    rule1.setBackgroundcolor( "252;255;205" );
    final TypeRenderingRule rule2 = new TypeRenderingRule();
    rule2.setMask( 32 );
    rule2.setBackgroundcolor( "201;255;189" );
    final Rules rules = new Rules();
    view.setRules( rules );
    rules.getRenderingrule().add( rule1 );
    rules.getRenderingrule().add( rule2 );
    return view;
  }

  private final static TypeAxis createDischargeAxis( )
  {
    return createAxis( "Q", "Abfluss", "double", "m3/s", TypePosition.LEFT, TypeDirection.VERTICAL, false );
  }

  private final static TypeAxis createDateAxis( )
  {
    return createAxis( "date", "Datum", "date", "", TypePosition.BOTTOM, TypeDirection.HORIZONTAL, false );
  }

  private final static TypeAxis createAxis( final String ID, final String label, final String dataType, final String unit, final TypePosition position, final TypeDirection direction, final boolean inverted )
  {
    final TypeAxis axis = new TypeAxis();
    axis.setInverted( inverted );
    axis.setPosition( position );
    axis.setDirection( direction );
    axis.setUnit( unit );
    axis.setLabel( label );
    axis.setDatatype( dataType );
    axis.setId( ID );
    return axis;
  }

  private final static TypeObservation createDiagObservation( final String zmlFilePath, final String obsTitle, final String obsID, final Color color, final Map<String, String> mappingsMap )
  {
    final TypeObservation observation = new TypeObservation();
    observation.setLinktype( "zml" );
    observation.setHref( zmlFilePath );
    final TypeCurve curve = new TypeCurve();
    observation.getCurve().add( curve );
    final String curveColor = new StringBuffer().append( color.getRed() ).append( ";" ).append( color.getGreen() ).append( ";" ).append( color.getBlue() ).toString();
    curve.setColor( curveColor );
    curve.setName( obsTitle );
    curve.setId( obsID );
    curve.setStroke( new TypeCurve.Stroke() );
    curve.getStroke().setWidth( 3.0f );
    curve.setShown( true );
    final List<TypeAxisMapping> mappings = curve.getMapping();
    for( final String key : mappingsMap.keySet() )
      mappings.add( createAxisMapping( key, mappingsMap.get( key ) ) );
    return observation;
  }

  private final static org.kalypso.template.obstableview.TypeObservation createTableObservation( final String zmlFilePath, final String obsTitle, final String obsID, final String axis )
  {
    final org.kalypso.template.obstableview.TypeObservation observation = new org.kalypso.template.obstableview.TypeObservation();
    observation.setLinktype( "zml" );
    observation.setHref( zmlFilePath );
    final TypeColumn column = new TypeColumn();
    column.setEditable( false );
    column.setFormat( "%.3f" );
    column.setWidth( 50 );
    column.setAxis( axis );
    column.setId( obsID );
    column.setName( obsTitle );
    observation.getColumn().add( column );
    return observation;
  }

  private final static TypeAxisMapping createAxisMapping( final String diagramAxis, final String observationAxis )
  {
    final TypeAxisMapping mapping = new TypeAxisMapping();
    mapping.setDiagramAxis( diagramAxis );
    mapping.setObservationAxis( observationAxis );
    return mapping;
  }

}
