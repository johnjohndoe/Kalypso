package org.kalypso.statistics.test;


public class ExampleGettingDataFor2DPlotting {
	public static void main(String[] args) {
		// example MBARI SOS
//		try {
//			URL urlGetObservation = new URL(
//					"http://mmisw.org/oostethys/sos?VERSION=1.0.0&SERVICE=SOS&REQUEST=GetObservation&offering=observationOffering_1455");
//			SOSObservationSweCommonReader reader = new SOSObservationSweCommonReader();
//			reader.setURLGetObservation(urlGetObservation);
//			String varURI = "http://mmisw.org/cf#pressure";
//			reader.process(varURI);
//			PlotData plotData = reader.getPlotData();
//			String title = plotData.getTitle();
//			long[] times = plotData.getTimes();
//			double[] values = plotData.getValues();
//			String unitsShort = plotData.getUnitsShort();
//			String unitsURL = plotData.getUnitsURI();
//			String variableURI = plotData.getVariableURI();
//			
//			System.out.println("title: "+title);
//			System.out.println("URI Variable: "+variableURI);
//			System.out.println("unitsShort: "+unitsShort);
//			System.out.println("FirstTimeStep: ");
//			System.out.println("   millisec: "+times[0]);
//			System.out.println("   values: "+values[0]);
//			
//
//		} catch (MalformedURLException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}

	}
	
	public static void main2(String[] args) {
		// example MBARI NG from IEEE 1451 using HTTP POST
//		try {
//			
//			String endPoint = "http://ww6.geoenterpriselab.com/SOSInterfaceToSTWS/STWS_SOS.asmx/GetCapabilities?request=GetCapabilities&service=SOS";
//			
//			URL urlGetObservation = new URL(
//					"http://mmisw.org/oostethys/sos?VERSION=1.0.0&SERVICE=SOS&REQUEST=GetObservation&offering=observationOffering_1455");
//			SOSObservationSweCommonReader reader = new SOSObservationSweCommonReader();
//			reader.setURLGetObservation(urlGetObservation);
//			String varURI = "http://mmisw.org/cf#pressure";
//			reader.process(varURI);
//			PlotData plotData = reader.getPlotData();
//			String title = plotData.getTitle();
//			long[] times = plotData.getTimes();
//			double[] values = plotData.getValues();
//			String unitsShort = plotData.getUnitsShort();
//			String unitsURL = plotData.getUnitsURI();
//			String variableURI = plotData.getVariableURI();
//			
//			System.out.println("title: "+title);
//			System.out.println("URI Variable: "+variableURI);
//			System.out.println("unitsShort: "+unitsShort);
//			System.out.println("FirstTimeStep: ");
//			System.out.println("   millisec: "+times[0]);
//			System.out.println("   values: "+values[0]);
//			
//
//		} catch (MalformedURLException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}

	}

}
