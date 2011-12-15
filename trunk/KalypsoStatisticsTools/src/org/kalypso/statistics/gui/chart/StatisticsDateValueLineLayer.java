package org.kalypso.statistics.gui.chart;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.TupleResult;

import de.openali.odysseus.chart.framework.model.layer.ILayerProvider;
import de.openali.odysseus.chart.framework.model.style.IStyleSet;

public class StatisticsDateValueLineLayer extends TupleResultLineLayer {

	private static final SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
	private static final String TOOLTIP_FORMAT = "%s: %s %n%s: %s";

	public StatisticsDateValueLineLayer(ILayerProvider provider, TupleResultDomainValueData<?, ?> data, IStyleSet styleSet) {
		super(provider, data, styleSet);
	}

	@Override
	protected String getTooltip(int index) {
		if (getValueData() == null)
			return "";
		final TupleResult tr = getValueData().getObservation().getResult();
		final int targetComponentIndex = tr.indexOfComponent(getValueData().getTargetComponentName());
		final int domainComponentIndex = tr.indexOfComponent(getValueData().getDomainComponentName());
		final String targetComponentLabel = ComponentUtilities.getComponentLabel(tr.getComponent(targetComponentIndex));
		final Object dateMillisObject = tr.get(index).getValue(domainComponentIndex);
		final String domainComponentLabel = "Date/time";
		final Object x;
		if (dateMillisObject instanceof Long) {
			x = SIMPLE_DATE_FORMAT.format(new Date((Long) dateMillisObject));
		}else{
			x = tr.get(index).getValue(domainComponentIndex);
		}
		final Object y = tr.get(index).getValue(targetComponentIndex);
		return String.format(TOOLTIP_FORMAT, new Object[] { domainComponentLabel, x, targetComponentLabel, y });
	}
}
