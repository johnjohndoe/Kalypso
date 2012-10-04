package org.kalypso.statistics.gui.chart;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.kalypso.chart.ext.observation.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.TupleResultLineLayer;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;

public class StatisticsDateValueLineLayer extends TupleResultLineLayer
{
  private static final SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat( "dd.MM.yyyy HH:mm:ss" );

  private static final String TOOLTIP_FORMAT = "%s: %s %n%s: %s";

  public StatisticsDateValueLineLayer( final ILayerProvider provider, final TupleResultDomainValueData< ? , ? > data, final IStyleSet styleSet )
  {
    super( provider, data, styleSet );
  }

  @Override
  protected String getTooltip( final IRecord record )
  {
    final TupleResult tr = record.getOwner();

    final int targetComponentIndex = tr.indexOfComponent( getValueData().getTargetComponentName() );
    final int domainComponentIndex = tr.indexOfComponent( getValueData().getDomainComponentName() );

    final String targetComponentLabel = ComponentUtilities.getComponentLabel( tr.getComponent( targetComponentIndex ) );

    final Object dateMillisObject = record.getValue( domainComponentIndex );
    final String domainComponentLabel = "Date/time";

    final Object x;
    if( dateMillisObject instanceof Long )
    {
      x = SIMPLE_DATE_FORMAT.format( new Date( (Long) dateMillisObject ) );
    }
    else
    {
      x = record.getValue( domainComponentIndex );
    }

    final Object y = record.getValue( targetComponentIndex );
    return String.format( TOOLTIP_FORMAT, new Object[] { domainComponentLabel, x, targetComponentLabel, y } );
  }
}