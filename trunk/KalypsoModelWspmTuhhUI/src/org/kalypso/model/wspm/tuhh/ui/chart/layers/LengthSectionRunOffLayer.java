package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.chart.ext.observation.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.TupleResultLineLayer;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.ext.base.layer.TooltipFormatter;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

//FIXME: why do we extend from TupleResultLineLayer -> implementation is totally different!
public class LengthSectionRunOffLayer extends TupleResultLineLayer
{
  public LengthSectionRunOffLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    if( valueData == null )
      return;

    final IObservation<TupleResult> observation = valueData.getObservation();
    if( observation == null )
      return;

    /* recreate hover info on every paint */
    clearInfoIndex();

    final List<Point> path = new ArrayList<>();

    final TupleResult result = observation.getResult();

    IRecord lastRecord = null;

    for( final IRecord record : result )
    {
      final Object domainValue = valueData.getDomainValue( record );
      final Object targetValue = valueData.getTargetValue( record );

      // FIXME: check if this is correct -> probably only correct, if painted in correct order (i.e. calculation order against flow direction)

      if( domainValue != null && targetValue != null )
      {
        final Point screen = getCoordinateMapper().logicalToScreen( domainValue, targetValue );

        addInfo( screen, domainValue, targetValue );

        if( lastRecord != null )
        {
          final Object lastDomain = valueData.getDomainValue( lastRecord );
          final Object lastTarget = valueData.getTargetValue( lastRecord );

          final Point lastScreen = getCoordinateMapper().logicalToScreen( lastDomain, lastTarget );

          final Point pathEnd = new Point( screen.x, lastScreen.y );
          path.add( pathEnd );

        }

        path.add( screen );

        lastRecord = record;
      }
    }

    paint( gc, path.toArray( new Point[] {} ) );
  }

  private void addInfo( final Point screen, final Object domainValue, final Object targetValue )
  {
    /* Format tooltip */
    final TupleResultDomainValueData< ? , ? > valueData = getValueData();
    final String stationLabel = ComponentUtilities.getComponentLabel( valueData.getDomainComponent() );
    final String targetLabel = ComponentUtilities.getComponentLabel( valueData.getTargetComponent() );

    final TooltipFormatter formatter = new TooltipFormatter( null );
    formatter.addLine( stationLabel, domainValue.toString() );
    formatter.addLine( targetLabel, targetValue.toString() );

    final String tooltip = formatter.format();

    /* Create info */
    final Rectangle hover = RectangleUtils.buffer( screen, 5 );

    // FIXME: lets have a nice figure!
    final IPaintable hoverFigure = null;

    final EditInfo info = new EditInfo( this, hoverFigure, null, null, tooltip, null );
    addInfoElement( hover, info );
  }
}