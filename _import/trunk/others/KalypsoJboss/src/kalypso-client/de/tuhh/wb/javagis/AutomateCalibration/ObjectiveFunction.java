package de.tuhh.wb.javagis.AutomateCalibration;

import java.util.TreeMap;
import java.util.Vector;
import java.util.Date;
import java.util.Iterator;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

// In dieser Klasse wird der Wert der Bewertungsfunktion berechnet, 
// es können 4 Bewertungsfunktionen mit entsprechenden Gewichtungen berücksichtigt werden:
// 1) Overall volume error,
// 2) Overall root mean square error (RMSE),
// 3) Average RMSE of peak flow events,
// 4) Average RMSE of low flow events.
public class ObjectiveFunction {
	private boolean equalWeights;

	private TreeMap qObs_calibration;
	private TreeMap qObs_calPeakFlows;
	private TreeMap qObs_calLowFlows;

	private boolean flagVolumeError;
	private boolean flagRootMeanSquareError;
	private boolean flagRootMeanSquareError_lowFlows;
	private boolean flagRootMeanSquareError_peakFlows;

	private double[] trafoConstants;

	Date startDate_calibration;
	Date endDate_calibration;
	double timeStep_calibration;

	double peakFlowLevel;
	double lowFlowLevel;
	
	int numPeakFlowEvents;
	int numLowFlowEvents;

	//Konstruktor zur Berechnung der Bewertungsfunktion 
	//mit gleicher Gewichtung der unterschiedlichen Bewertungen
	public ObjectiveFunction(
		TreeMap qObs_all,
		boolean[] functions,
		double peakFlowLevel,
		double lowFlowLevel,
		Date startDate,
		Date endDate,
		double timeStep,
		TreeMap qSim_Initial) {

		this.flagVolumeError = functions[0];
		this.flagRootMeanSquareError = functions[1];
		this.flagRootMeanSquareError_peakFlows = functions[2];
		this.flagRootMeanSquareError_lowFlows = functions[3];

		this.equalWeights = true;

		this.peakFlowLevel = peakFlowLevel;
		this.lowFlowLevel = lowFlowLevel;

		this.startDate_calibration = startDate;
		this.endDate_calibration = endDate;
		this.timeStep_calibration = timeStep;

		this.qObs_calibration = getDischarge(qObs_all);
		if (flagRootMeanSquareError_peakFlows) {
			this.qObs_calPeakFlows = getDischarge_peakFlows(qObs_calibration);
			this.numPeakFlowEvents = countEvents(qObs_calPeakFlows);
		}
		if (flagRootMeanSquareError_lowFlows) {
			this.qObs_calLowFlows = getDischarge_lowFlows(qObs_calibration);
			this.numLowFlowEvents = countEvents(qObs_calLowFlows);
		}

		calculateEqualWeights(qSim_Initial);

	}

	//Konstruktor zur Berechnung der Bewertungsfunktion 
	//unter Vorgabe der Transformationskonstanten Ai der unterschiedlichen Bewertungen
	public ObjectiveFunction(
		TreeMap qObs_all,
		boolean[] functions,
		double[] trafoConstants,
		double peakFlowLevel,
		double lowFlowLevel,
		Date startDate,
		Date endDate,
		double timeStep) {

		this.trafoConstants = trafoConstants;

		this.flagVolumeError = functions[0];
		this.flagRootMeanSquareError = functions[1];
		this.flagRootMeanSquareError_peakFlows = functions[2];
		this.flagRootMeanSquareError_lowFlows = functions[3];

		this.equalWeights = false;

		this.peakFlowLevel = peakFlowLevel;
		this.lowFlowLevel = lowFlowLevel;

		this.startDate_calibration = startDate;
		this.endDate_calibration = endDate;
		this.timeStep_calibration = timeStep;

		this.qObs_calibration = getDischarge(qObs_all);
		System.out.println("***Observed Discharge (Calibration)***");
		Iterator it_all = qObs_calibration.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Date actualDate = (Date) dateKey;
			Object value = (String) qObs_calibration.get(dateKey);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			System.out.println(
				"Actual Date: "
					+ dateformat.format(actualDate)
					+ "   Value: "
					+ value);
		}
		if (flagRootMeanSquareError_peakFlows) {
			this.qObs_calPeakFlows = getDischarge_peakFlows(qObs_calibration);
			this.numPeakFlowEvents = countEvents(qObs_calPeakFlows);
			System.out.println("***Observed Discharge (PeakFlows)***");
			it_all = qObs_calPeakFlows.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				Date actualDate = (Date) dateKey;
				Object value = (String) qObs_calPeakFlows.get(dateKey);
				DateFormat dateformat =
					new SimpleDateFormat("dd.MM.yyyy HH:mm");
				dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
				System.out.println(
					"Actual Date: "
						+ dateformat.format(actualDate)
						+ "   Value: "
						+ value);
			}
		}
		if (flagRootMeanSquareError_lowFlows) {
			this.qObs_calLowFlows = getDischarge_lowFlows(qObs_calibration);
			this.numLowFlowEvents = countEvents(qObs_calLowFlows);
			System.out.println("***Observed Discharge (LowFlows)***");
			it_all = qObs_calLowFlows.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				Date actualDate = (Date) dateKey;
				Object value = (String) qObs_calLowFlows.get(dateKey);
				DateFormat dateformat =
					new SimpleDateFormat("dd.MM.yyyy HH:mm");
				dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
				System.out.println(
					"Actual Date: "
						+ dateformat.format(actualDate)
						+ "   Value: "
						+ value);
			}
		}

	}

	public double getObjectiveFunctionValue(TreeMap qSim_all) {
		double functionValue = 0;

		double volumeError = calculateVolumeError(qObs_calibration, qSim_all);
		double rootMeanSquareError =
			calculateRootMeanSquareError(qObs_calibration, qSim_all);
		double rootMeanSquareError_peakFlows =
			calculateRootMeanSquareError_peakFlows(qObs_calPeakFlows, qSim_all);
		double rootMeanSquareError_lowFlows =
			calculateRootMeanSquareError_lowFlows(qObs_calLowFlows, qSim_all);

		int numFunctions = 4;
		double[] functionValues = new double[numFunctions];
		functionValues[0] = volumeError;
		functionValues[1] = rootMeanSquareError;
		functionValues[2] = rootMeanSquareError_peakFlows;
		functionValues[3] = rootMeanSquareError_lowFlows;

		for (int i = 0; i < numFunctions; i++) {
			double actualTrafoConstant = trafoConstants[i];
			double x = functionValues[i] + actualTrafoConstant;
			double actualFunctionValue = Math.pow(x, 2);
			functionValue = functionValue + actualFunctionValue;
		}
		functionValue = Math.sqrt(functionValue);

		return functionValue;
	}

	private double calculateVolumeError(
		TreeMap qObs_calibration,
		TreeMap qSim_all) {
		double volumeError = 0;
		if (flagVolumeError) {
			Iterator it_all = qObs_calibration.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				double dischargeValue_obs =
					Double.parseDouble((String) qObs_calibration.get(dateKey));
				double dischargeValue_sim =
					Double.parseDouble((String) qSim_all.get(dateKey));
				double x = dischargeValue_obs - dischargeValue_sim;
				volumeError = volumeError + x;
			}
			volumeError = volumeError / qObs_calibration.size();
		} else {
			volumeError = 0;
		}
		return volumeError;
	}
	private double calculateRootMeanSquareError(
		TreeMap qObs_calibration,
		TreeMap qSim_all) {
		double rootMeanSquareError = 0;
		if (flagRootMeanSquareError) {
			Iterator it_all = qObs_calibration.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				double dischargeValue_obs =
					Double.parseDouble((String) qObs_calibration.get(dateKey));
				double dischargeValue_sim =
					Double.parseDouble((String) qSim_all.get(dateKey));
				double x = dischargeValue_obs - dischargeValue_sim;
				double pow = Math.pow(x, 2);
				rootMeanSquareError = rootMeanSquareError + pow;
			}
			rootMeanSquareError = rootMeanSquareError / qObs_calibration.size();
			rootMeanSquareError = Math.sqrt(rootMeanSquareError);
		} else {
			rootMeanSquareError = 0;
		}
		return rootMeanSquareError;
	}
	private double calculateRootMeanSquareError_peakFlows(
		TreeMap qObs_calPeakFlows,
		TreeMap qSim_all) {
		double rootMeanSquareError_peakFlows = 0;
		if (flagRootMeanSquareError_peakFlows
			&& qObs_calPeakFlows.size() != 0) {
			Iterator it_all = qObs_calPeakFlows.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				double dischargeValue_obs =
					Double.parseDouble((String) qObs_calPeakFlows.get(dateKey));
				double dischargeValue_sim =
					Double.parseDouble((String) qSim_all.get(dateKey));
				double x = dischargeValue_obs - dischargeValue_sim;
				double pow = Math.pow(x, 2);
				rootMeanSquareError_peakFlows =
					rootMeanSquareError_peakFlows + pow;
			}
			rootMeanSquareError_peakFlows =
				rootMeanSquareError_peakFlows / qObs_calPeakFlows.size();
			rootMeanSquareError_peakFlows =
				Math.sqrt(rootMeanSquareError_peakFlows);
			//TODO{count number of peak flow events and change calculation}
		} else {
			rootMeanSquareError_peakFlows = 0;
		}
		return rootMeanSquareError_peakFlows;
	}
	private double calculateRootMeanSquareError_lowFlows(
		TreeMap qObs_calLowFlows,
		TreeMap qSim_all) {
		double rootMeanSquareError_lowFlows = 0;
		if (flagRootMeanSquareError_lowFlows && qObs_calLowFlows.size() != 0) {
			Iterator it_all = qObs_calLowFlows.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				double dischargeValue_obs =
					Double.parseDouble((String) qObs_calLowFlows.get(dateKey));
				double dischargeValue_sim =
					Double.parseDouble((String) qSim_all.get(dateKey));
				double x = dischargeValue_obs - dischargeValue_sim;
				double pow = Math.pow(x, 2);
				rootMeanSquareError_lowFlows =
					rootMeanSquareError_lowFlows + pow;
			}
			rootMeanSquareError_lowFlows =
				rootMeanSquareError_lowFlows / qObs_calLowFlows.size();
			rootMeanSquareError_lowFlows =
				Math.sqrt(rootMeanSquareError_lowFlows);
			//TODO{count number of low flow events and change calculation}
		} else {
			rootMeanSquareError_lowFlows = 0;
		}
		return rootMeanSquareError_lowFlows;
	}

	// Ermittlung aller Ablussdaten für den Kalibrierungszeitraum
	private TreeMap getDischarge(TreeMap data) {

		TreeMap resultData = new TreeMap();
		DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
		dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));

		Date tempDate = new Date(startDate_calibration.getTime());
		long startDateAsLong = startDate_calibration.getTime();
		long timeStepAsLong;
		if (timeStep_calibration == 0.083) {
			timeStepAsLong = 300000l;
			System.out.println("TimeStepAsLong: " + timeStepAsLong);
		} else {
			timeStepAsLong =
				((long) ((float) timeStep_calibration * 1000f)) * 3600l;
		}
		int index = 0;
		while ((tempDate.compareTo(endDate_calibration)) <= 0) {
			index = index + 1;
			try {
				Object dischargeValue = data.get(tempDate);
				System.out.println(
					"Temp Date: "
						+ dateformat.format(tempDate)
						+ ", Discharge: "
						+ dischargeValue);
				Date storeDate = new Date(tempDate.getTime());
				resultData.put(storeDate, dischargeValue);
			} catch (Exception e) {
				//e.printStackTrace();
				System.out.println(
					"ObservedDischargeValue for Date: "
						+ dateformat.format(tempDate)
						+ " does not exist");
			}
			startDateAsLong = startDateAsLong + timeStepAsLong;
			tempDate.setTime(startDateAsLong);
			//System.out.println("Temp Date: " + dateformat.format(tempDate));
		}
		return resultData;
	}

	private TreeMap getDischarge_peakFlows(TreeMap qCalibration) {
		TreeMap qObs_peakFlows = new TreeMap();
		Iterator it_all = qCalibration.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Double dischargeValue =
				new Double((String) qCalibration.get(dateKey));
			if (dischargeValue.doubleValue() > peakFlowLevel) {
				qObs_peakFlows.put(dateKey, qCalibration.get(dateKey));
			}
		}
		return qObs_peakFlows;
	}

	private TreeMap getDischarge_lowFlows(TreeMap qCalibration) {
		TreeMap qObs_lowFlows = new TreeMap();
		Iterator it_all = qCalibration.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Double dischargeValue =
				new Double((String) qCalibration.get(dateKey));
			if (dischargeValue.doubleValue() < lowFlowLevel) {
				qObs_lowFlows.put(dateKey, qCalibration.get(dateKey));
			}
		}
		return qObs_lowFlows;
	}

	private int countEvents(TreeMap discharge) {
		int numEvents = 0;
		if (discharge.size() > 0) {
			numEvents = 1;
		}
		long timeStepAsLong;
		if (timeStep_calibration == 0.083) {
			timeStepAsLong = 300000l;
			System.out.println("TimeStepAsLong: " + timeStepAsLong);
		} else {
			timeStepAsLong =
				((long) ((float) timeStep_calibration * 1000f)) * 3600l;
		}
		Object[] keys = new Object[discharge.size()];
		keys = (discharge.keySet()).toArray();
		for (int i = 0; i < keys.length-1; i++) {
			System.out.println("i="+i);
			long date_i = ((Date)keys[i]).getTime();
			long date_ii = ((Date)keys[i+1]).getTime();
			System.out.println("date i: "+(Date)keys[i]+", date i+1: "+(Date)keys[i+1]);
			long tempTimeStep = date_ii-date_i;
			if(tempTimeStep!=timeStepAsLong){
				numEvents = numEvents+1;
			}
		}
		System.out.println("Number of Events: "+numEvents);
		return numEvents;
	}

	private void calculateEqualWeights(TreeMap qSim_Initial) {

		System.out.println("***Observed Discharge (Calibration)***");
		Iterator it_all = qObs_calibration.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Date actualDate = (Date) dateKey;
			Object value = (String) qObs_calibration.get(dateKey);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			System.out.println(
				"Actual Date: "
					+ dateformat.format(actualDate)
					+ "   Value: "
					+ value);
		}
		/*System.out.println("***Simulated Discharge (Initial Values)***");
		it_all = qSim_Initial.keySet().iterator();
		while (it_all.hasNext()) {
			Object dateKey = it_all.next();
			Date actualDate = (Date) dateKey;
			Object value = (String) qSim_Initial.get(dateKey);
			DateFormat dateformat = new SimpleDateFormat("dd.MM.yyyy HH:mm");
			dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
			System.out.println(
				"Actual Date: "
					+ dateformat.format(actualDate)
					+ "   Value: "
					+ value);
		}*/

		Double[] minFunctionValues = new Double[4];
		Vector minFuncValues = new Vector();
		if (flagVolumeError) {
			Double functionValue =
				new Double(
					calculateVolumeError(qObs_calibration, qSim_Initial));
			minFunctionValues[0] = functionValue;
			minFuncValues.addElement(functionValue);
		}
		if (flagRootMeanSquareError) {
			Double functionValue =
				new Double(
					calculateRootMeanSquareError(
						qObs_calibration,
						qSim_Initial));
			minFunctionValues[1] = functionValue;
			minFuncValues.addElement(functionValue);
		}
		if (flagRootMeanSquareError_peakFlows) {
			System.out.println("***Observed Discharge (Peak Flows)***");
			it_all = qObs_calPeakFlows.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				Date actualDate = (Date) dateKey;
				Object value = (String) qObs_calPeakFlows.get(dateKey);
				DateFormat dateformat =
					new SimpleDateFormat("dd.MM.yyyy HH:mm");
				dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
				System.out.println(
					"Actual Date: "
						+ dateformat.format(actualDate)
						+ "   Value: "
						+ value);
			}

			Double functionValue =
				new Double(
					calculateRootMeanSquareError_peakFlows(
						qObs_calPeakFlows,
						qSim_Initial));
			minFunctionValues[2] = functionValue;
			minFuncValues.addElement(functionValue);
		}
		if (flagRootMeanSquareError_lowFlows) {
			System.out.println("***Observed Discharge (Low Flows)***");
			it_all = qObs_calLowFlows.keySet().iterator();
			while (it_all.hasNext()) {
				Object dateKey = it_all.next();
				Date actualDate = (Date) dateKey;
				Object value = (String) qObs_calLowFlows.get(dateKey);
				DateFormat dateformat =
					new SimpleDateFormat("dd.MM.yyyy HH:mm");
				dateformat.setTimeZone(TimeZone.getTimeZone("GMT+1:00"));
				System.out.println(
					"Actual Date: "
						+ dateformat.format(actualDate)
						+ "   Value: "
						+ value);
			}

			Double functionValue =
				new Double(
					calculateRootMeanSquareError_lowFlows(
						qObs_calLowFlows,
						qSim_Initial));
			minFunctionValues[3] = functionValue;
			minFuncValues.addElement(functionValue);
		}

		System.out.println("Min Function Values: ");
		for (int n = 0; n < minFunctionValues.length; n++) {
			System.out.println(minFunctionValues[n]);
		}

		//find maximum value of minFuncValues
		double max = ((Double) minFuncValues.elementAt(0)).doubleValue();
		for (int i = 1; i < minFuncValues.size(); i++) {
			double actualValue =
				((Double) minFuncValues.elementAt(i)).doubleValue();
			if (actualValue > max) {
				max = actualValue;
			}
		}

		System.out.println("Maximum function value: " + max);

		trafoConstants = new double[minFunctionValues.length];
		for (int k = 0; k < minFunctionValues.length; k++) {
			if (minFunctionValues[k] != null) {
				trafoConstants[k] = max - minFunctionValues[k].doubleValue();
			} else {
				trafoConstants[k] = 0;
			}
		}

		System.out.println("Transformation Constants: ");
		for (int n = 0; n < trafoConstants.length; n++) {
			System.out.println(trafoConstants[n]);
		}

	}

}
