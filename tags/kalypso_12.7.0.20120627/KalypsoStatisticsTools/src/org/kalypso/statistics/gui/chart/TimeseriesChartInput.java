package org.kalypso.statistics.gui.chart;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IPersistableElement;
import org.kalypso.chart.ui.internal.workbench.IDatabaseStorageEditorInput;
import org.kalypso.contribs.eclipse.core.resources.StringStorage;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.statistics.types.ETimeseriesType;
import org.kalypso.statistics.types.data.TimeserieProfile;

import de.openali.odysseus.chartconfig.x020.AxisRendererType;
import de.openali.odysseus.chartconfig.x020.AxisType;
import de.openali.odysseus.chartconfig.x020.AxisType.Direction;
import de.openali.odysseus.chartconfig.x020.AxisType.PreferredAdjustment;
import de.openali.odysseus.chartconfig.x020.ChartConfigurationDocument;
import de.openali.odysseus.chartconfig.x020.ChartConfigurationType;
import de.openali.odysseus.chartconfig.x020.ChartType;
import de.openali.odysseus.chartconfig.x020.ChartType.Mappers;
import de.openali.odysseus.chartconfig.x020.ChartType.Renderers;
import de.openali.odysseus.chartconfig.x020.LayerType;
import de.openali.odysseus.chartconfig.x020.LayerType.MapperRefs;
import de.openali.odysseus.chartconfig.x020.LayersType;
import de.openali.odysseus.chartconfig.x020.LineStyleType;
import de.openali.odysseus.chartconfig.x020.ParameterType;
import de.openali.odysseus.chartconfig.x020.ParametersType;
import de.openali.odysseus.chartconfig.x020.PositionType;
import de.openali.odysseus.chartconfig.x020.ProviderType;
import de.openali.odysseus.chartconfig.x020.StylesDocument.Styles;

public class TimeseriesChartInput implements IDatabaseStorageEditorInput {

	private final List<TimeserieProfile> m_timeserieProfiles = new ArrayList<TimeserieProfile>();
	private final String m_name;
	private String m_storageContent = null;

	public TimeseriesChartInput(String name, TimeserieProfile timeserieProfile) {
		m_name = name;
		m_timeserieProfiles.add(timeserieProfile);
	}

	public TimeseriesChartInput(String name, List<TimeserieProfile> timeserieProfiles) {
		m_name = name;
		m_timeserieProfiles.addAll(timeserieProfiles);
	}

	@Override
	public boolean exists() {
		return false;
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		return null;
	}

	@Override
	public String getName() {
		return m_name;
	}

	@Override
	public IPersistableElement getPersistable() {
		return null;
	}

	@Override
	public String getToolTipText() {
		return "";
	}

	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") Class arg0) {
		return null;
	}

	@Override
	public IStorage getStorage() throws CoreException {
		createStorageContent();
		return new StringStorage(m_storageContent, null);
	}

	private void createStorageContent() {
		if (m_storageContent != null) {
			return;
		}
		ChartConfigurationDocument document = ChartConfigurationDocument.Factory.newInstance();
		ChartConfigurationType configuration = document.addNewChartConfiguration();
		ChartType chart = configuration.addNewChart();
		chart.addNewTitle().setStringValue(m_name);
		LayersType layers = chart.addNewLayers();

		// collect differentTypes of value axes needed
		final Map<ETimeseriesType, String> axesMap = new HashMap<ETimeseriesType, String>();
		for (TimeserieProfile profile : m_timeserieProfiles) {
			axesMap.put(profile.getTimeseriesType(), String.format("valueAxis_%s", profile.getTimeseriesType().getAbbreviation()));
		}

		for (TimeserieProfile profile : m_timeserieProfiles) {
			LayerType layer = layers.addNewLayer();
			layer.setVisible(true);
			layer.setId("UID_" + profile.getUID());
			layer.setTitle(profile.getName());
			ProviderType provider = layer.addNewProvider();
			provider.setEpid(StatisticsDBChartLayerProvider.ID);
			ParametersType parameters = provider.addNewParameters();
			ParameterType paramUID = parameters.addNewParameter();
			paramUID.setName("uid");
			paramUID.setValue(Integer.toString(profile.getUID()));
			ParameterType paramFrom = parameters.addNewParameter();
			paramFrom.setName("from");
			paramFrom.setValue(Long.toString(profile.getTimeFrom().getTimeInMillis()));
			ParameterType paramTo = parameters.addNewParameter();
			paramTo.setName("to");
			paramTo.setValue(Long.toString(profile.getTimeTo().getTimeInMillis()));
			ParameterType paramType = parameters.addNewParameter();
			paramType.setName("type");
			paramType.setValue(profile.getTimeseriesType().name());

			ParameterType paramDomainComponentID = parameters.addNewParameter();
			paramDomainComponentID.setName("domainComponentId");
			paramDomainComponentID.setValue(ITimeseriesConstants.TYPE_DATE);
			ParameterType paramTargetComponentID = parameters.addNewParameter();
			paramTargetComponentID.setName("targetComponentId");
			paramTargetComponentID.setValue(profile.getTimeseriesType().getAbbreviation());

			MapperRefs mapperRefs = layer.addNewMapperRefs();
			mapperRefs.addNewDomainAxisRef().setRef("dateAxis");
			mapperRefs.addNewTargetAxisRef().setRef(axesMap.get(profile.getTimeseriesType()));

			createStyle(layer, profile.getTimeseriesType());
		}
		Mappers mappers = chart.addNewMappers();
		AxisType dateAxis = mappers.addNewAxis();
		dateAxis.setId("dateAxis");
		dateAxis.setLabel("Date");
		dateAxis.addNewRendererRef().setRef("AxisDateRenderer");
		ProviderType dateAxisProvider = dateAxis.addNewProvider();
		dateAxisProvider.setEpid("de.openali.odysseus.chart.ext.axis.provider.GenericLinear");
		dateAxis.addNewAxisNumberRangeRestriction().setMinRange(new BigDecimal(1000.0));
		PreferredAdjustment dateAxisPreferredAdjustment = dateAxis.addNewPreferredAdjustment();
		dateAxisPreferredAdjustment.setBefore(0);
		dateAxisPreferredAdjustment.setAfter(0);
		dateAxisPreferredAdjustment.setRange(100);

		for (ETimeseriesType type : axesMap.keySet()) {
			AxisType valueAxis = mappers.addNewAxis();
			valueAxis.setId(axesMap.get(type));
			if (type.getUnit().trim().length() > 0) {
				valueAxis.setLabel(String.format("%s [%s]", type.getLabel(), type.getUnit()));
			} else {
				valueAxis.setLabel(type.getLabel());
			}
			valueAxis.addNewRendererRef().setRef("AxisDoubleRenderer");

			switch (type) {
			case PRECIPITATION:
			case PRECIPITATION_EXTREMS:
				dateAxis.setPosition(PositionType.TOP);
				dateAxis.setDirection(Direction.POSITIVE);
				valueAxis.setPosition(PositionType.LEFT);
				valueAxis.setDirection(Direction.NEGATIVE);
				break;
			default:
				dateAxis.setPosition(PositionType.BOTTOM);
				dateAxis.setDirection(Direction.POSITIVE);
				valueAxis.setPosition(PositionType.LEFT);
				valueAxis.setDirection(Direction.POSITIVE);
				break;
			}
			ProviderType valueAxisProvider = valueAxis.addNewProvider();
			valueAxisProvider.setEpid("de.openali.odysseus.chart.ext.axis.provider.GenericLinear");
		}

		Renderers axisRenderers = chart.addNewRenderers();
		AxisRendererType axisDoubleRenderer = axisRenderers.addNewAxisRenderer();
		axisDoubleRenderer.setId("AxisDoubleRenderer");
		ProviderType axisDoubleRendererProvider = axisDoubleRenderer.addNewProvider();
		axisDoubleRendererProvider.setEpid("de.openali.odysseus.chart.ext.base.axisrenderer.provider.GenericNumberAxisRendererProvider");
		ParametersType axisRendererProviderParameters = axisDoubleRendererProvider.addNewParameters();
		setParameter(axisRendererProviderParameters.addNewParameter(), "inset_tick", "2");
		setParameter(axisRendererProviderParameters.addNewParameter(), "inset_label", "2");
		setParameter(axisRendererProviderParameters.addNewParameter(), "tick_length", "2");
		setParameter(axisRendererProviderParameters.addNewParameter(), "hide_cut", "false");
		setParameter(axisRendererProviderParameters.addNewParameter(), "gap", "0");

		AxisRendererType axisDateRenderer = axisRenderers.addNewAxisRenderer();
		axisDateRenderer.setId("AxisDateRenderer");
		ProviderType axisDateRendererProvider = axisDateRenderer.addNewProvider();
		axisDateRendererProvider.setEpid("de.openali.odysseus.chart.ext.base.axisrenderer.provider.DateTimeAxisRendererProvider");
		ParametersType axisDateRendererProviderParameters = axisDateRendererProvider.addNewParameters();
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "inset_tick", "2");
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "inset_label", "2");
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "tick_length", "2");
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "min_tick_interval", "86400000");
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "hide_cut", "false");
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "gap", "0");
		setParameter(axisDateRendererProviderParameters.addNewParameter(), "label_position", "CENTER");

		m_storageContent = document.xmlText();

		// XmlOptions options = new XmlOptions();
		// options.setSavePrettyPrint();
		// System.out.println(document.xmlText(options));
		
	}

	private void setParameter(ParameterType parameter, String name, String value) {
		parameter.setName(name);
		parameter.setValue(value);
	}

	private void createStyle(LayerType layer, ETimeseriesType timeseriesType) {
		Styles styles = layer.addNewStyles();
		// switch (timeseriesType) {
		// case PRECIPITATION:
		// case PRECIPITATION_EXTREMS:
		// Color colorN = Display.getDefault().getSystemColor(SWT.COLOR_BLUE);
		// AreaStyleType areaStyle = styles.addNewAreaStyle();
		// areaStyle.setAlpha(new byte[] { 64 });
		// FillType fill = areaStyle.addNewFill();
		// fill.addNewColorFill().setColor(new byte[] { (byte) colorN.getRed(),
		// (byte) colorN.getGreen(), (byte) colorN.getBlue() });
		// StrokeType stroke = areaStyle.addNewStroke();
		// stroke.setWidth(1);
		// stroke.setLineColor(new byte[] { (byte) colorN.getRed(), (byte)
		// colorN.getGreen(), (byte) colorN.getBlue() });
		// break;
		// case WATERLEVEL:
		LineStyleType lineStyle = styles.addNewLineStyle();
		lineStyle.setRole("line");
		lineStyle.setWidth(2);
		Color colorW = Display.getDefault().getSystemColor(SWT.COLOR_BLUE);
		lineStyle.setLineColor(new byte[] { (byte) colorW.getRed(), (byte) colorW.getGreen(), (byte) colorW.getBlue() });
		// break;
		// default:
		// break;
		// }
	}

}
