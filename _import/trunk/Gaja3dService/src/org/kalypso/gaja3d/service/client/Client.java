package org.kalypso.gaja3d.service.client;

import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.apache.axis.message.MessageElement;
import org.apache.axis.message.addressing.Address;
import org.apache.axis.message.addressing.EndpointReferenceType;
import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.globus.wsrf.encoding.ObjectDeserializer;
import org.globus.wsrf.encoding.ObjectSerializer;
import org.globus.wsrf.encoding.SerializationException;
import org.kalypso.gaja3d.service.Gaja3DServiceAddressingLocator;
import org.kalypso.gaja3d.service.factory.Gaja3DResourceFactoryServiceAddressingLocator;
import org.kalypso.gaja3d.service.factory.stubs.CreateResource;
import org.kalypso.gaja3d.service.factory.stubs.CreateResourceResponse;
import org.kalypso.gaja3d.service.factory.stubs.Gaja3DResourceFactoryPortType;
import org.kalypso.gaja3d.service.impl.Gaja3dQNames;
import org.kalypso.gaja3d.service.internal.CodeTypeUtil;
import org.kalypso.gaja3d.service.stubs.Boundary;
import org.kalypso.gaja3d.service.stubs.Breaklines;
import org.kalypso.gaja3d.service.stubs.CreateGridParametersType;
import org.kalypso.gaja3d.service.stubs.CreateTinParametersType;
import org.kalypso.gaja3d.service.stubs.DemGrid;
import org.kalypso.gaja3d.service.stubs.DemPoints;
import org.kalypso.gaja3d.service.stubs.DetectBreaklinesParametersType;
import org.kalypso.gaja3d.service.stubs.Gaja3DPortType;
import org.kalypso.gaja3d.service.stubs.GridX;
import org.kalypso.gaja3d.service.stubs.GridY;
import org.kalypso.gaja3d.service.stubs.Identifier;
import org.kalypso.gaja3d.service.stubs.MaxArea;
import org.kalypso.gaja3d.service.stubs.MinAngle;
import org.kalypso.gaja3d.service.stubs.SmoothFilter;
import org.kalypso.gaja3d.service.stubs.SmoothFilterMethod;
import org.oasis.wsrf.lifetime.Destroy;
import org.oasis.wsrf.properties.GetMultipleResourcePropertiesResponse;
import org.oasis.wsrf.properties.GetMultipleResourceProperties_Element;
import org.oasis.wsrf.properties.SetResourceProperties_Element;
import org.oasis.wsrf.properties.UpdateType;
import org.xml.sax.InputSource;

/**
 * This client creates a new Gaja3dService instance through a
 * Gaja3dResourceFactoryService.
 */
public class Client {

	private static final String SERVICE_FACTORY_URI = "http://localhost/services/Gaja3dResourceFactoryService";
	private static final String EPR_FILENAME = "test.epr";
	private static final boolean SAVE_EPR = true;
	private static final boolean USE_SAVED_EPR = false;

	private static final String BOUNDARY_FILENAME = "resources/boundary.zip";
	private static final String DEMPOINTS_FILENAME = "resources/allpoints.zip";

	/*
	 * for skipping grid creation
	 */
	private static final String DEMGRID_FILENAME = "resources/DemGrid.asc";
	
	/*
	 * for skipping breakline creation
	 */
	private static final String BREAKLINES_FILENAME = "resources/breaklines.zip";

	public static void main(String[] args) throws RemoteException,
			ServiceException {
		final EndpointReferenceType instanceEPR;
		if (USE_SAVED_EPR) {
			try {
				final InputSource inputSource = new InputSource(
						new FileInputStream(EPR_FILENAME));
				instanceEPR = (EndpointReferenceType) ObjectDeserializer
						.deserialize(inputSource, EndpointReferenceType.class);
			} catch (final Exception e) {
				e.printStackTrace();
				return;
			}
		} else {
			final Gaja3DResourceFactoryServiceAddressingLocator factoryLocator = new Gaja3DResourceFactoryServiceAddressingLocator();
			final EndpointReferenceType factoryEPR = new EndpointReferenceType();
			try {
				final Address address = new Address(SERVICE_FACTORY_URI);
				factoryEPR.setAddress(address);
			} catch (final MalformedURIException e) {
				e.printStackTrace();
			}

			Gaja3DResourceFactoryPortType mathFactory;
			try {
				mathFactory = factoryLocator
						.getGaja3dResourceFactoryPortTypePort(factoryEPR);
			} catch (final ServiceException e) {
				e.printStackTrace();
				return;
			}

			// create instance
			final CreateResource request = new CreateResource();
			final CreateResourceResponse createResponse = mathFactory
					.createResource(request);
			instanceEPR = createResponse.getEndpointReference();
		}

		// save epr if desired
		if (SAVE_EPR)
			writeEPRtoFile(instanceEPR);

		// retrieve instance
		final Gaja3DServiceAddressingLocator instanceLocator = new Gaja3DServiceAddressingLocator();
		final Gaja3DPortType gaja3d = instanceLocator
				.getGaja3dPortTypePort(instanceEPR);
		System.out.println("Retrieved service instance.");
		setSecurity(gaja3d);
		/*
		 * // call GetCapabilities final Capabilities capabilities =
		 * callGetCapabilities(gaja3d); // call DescribeProcess for all offered
		 * processes callDescribeProcess(gaja3d, capabilities);
		 */

		// set Boundary RP
		final SetResourceProperties_Element setResourcePropertiesRequest = buildSetBoundaryResourceProperty();
		gaja3d.setResourceProperties(setResourcePropertiesRequest);
		printResourceProperties(gaja3d);

		// call Execute_createGrid
//		final CreateGridParametersType execute_createGrid = buildExecuteCreateGrid();
//		gaja3d.execute_createGrid(execute_createGrid);
//		printResourceProperties(gaja3d);

		// call Execute_detectBreaklines
		// final DetectBreaklinesParametersType detectBreaklinesParameters =
		// buildExecuteDetectBreaklines();
		// gaja3d.execute_detectBreaklines(detectBreaklinesParameters);
		// printResourceProperties(gaja3d);
		
		// call Execute_createTin
		final CreateTinParametersType createTinParameters = buildExecuteCreateTin();
		gaja3d.execute_createTin(createTinParameters);
		printResourceProperties(gaja3d);

		if (!SAVE_EPR) {
			// call Destroy
			final Destroy destroyRequest = new Destroy();
			gaja3d.destroy(destroyRequest);
			System.out.println("Destroyed instance.");
		}

	}
	
	private static CreateTinParametersType buildExecuteCreateTin() {
		final CreateTinParametersType execute_createTin = new CreateTinParametersType();
		/*
		 * remove comment when skipping breakline detection
		 */
		try {
//			final Boundary boundary = new Boundary();
//			boundary.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
//					Gaja3dQNames.RP_BOUNDARY));
//			final URI boundaryHref = new URI(Client.class.getResource(
//					BOUNDARY_FILENAME).toURI().toString());
//			boundary.setHref(boundaryHref);
//			execute_createTin.setBoundary(boundary);
			
			final Breaklines breaklines = new Breaklines();
			breaklines.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_BREAKLINES));
			final URI breaklinesHref = new URI(Client.class.getResource(
					BREAKLINES_FILENAME).toURI().toString());
			breaklines.setHref(breaklinesHref);
			execute_createTin.setBreaklines(breaklines);
		} catch (final MalformedURIException e) {
			e.printStackTrace();
		} catch (final URISyntaxException e) {
			e.printStackTrace();
		}

//		final MinAngle minAngle = new MinAngle();
//		minAngle.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
//				Gaja3dQNames.RP_MIN_ANGLE));
//		minAngle.setAngle(1000);
//		execute_createTin.setMinAngle(minAngle);
//		
//		final MaxArea maxArea = new MaxArea();
//		maxArea.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
//				Gaja3dQNames.RP_MAX_AREA));
//		maxArea.setArea(1000);
//		execute_createTin.setMaxArea(maxArea);
		

		return execute_createTin;
	}

	private static DetectBreaklinesParametersType buildExecuteDetectBreaklines() {
		final DetectBreaklinesParametersType execute_detectBreaklines = new DetectBreaklinesParametersType();
		/*
		 * remove comment for skipping grid creation
		 */
		try {
			final Boundary boundary = new Boundary();
			boundary.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_BOUNDARY));
			final URI boundaryHref = new URI(Client.class.getResource(
					BOUNDARY_FILENAME).toURI().toString());
			boundary.setHref(boundaryHref);
			execute_detectBreaklines.setBoundary(boundary);

			final DemGrid demGrid = new DemGrid();
			demGrid.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
					Gaja3dQNames.RP_DEM_GRID));
			final URI demGridHref = new URI(Client.class.getResource(
					DEMGRID_FILENAME).toURI().toString());
			demGrid.setHref(demGridHref);
			execute_detectBreaklines.setDemGrid(demGrid);
		} catch (final MalformedURIException e) {
			e.printStackTrace();
		} catch (final URISyntaxException e) {
			e.printStackTrace();
		}
		// ////

		final SmoothFilter smoothFilter = new SmoothFilter();
		smoothFilter.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
				Gaja3dQNames.RP_SMOOTH_FILTER));
		final SmoothFilterMethod method = SmoothFilterMethod.gauss;
		smoothFilter.setMethod(method);
		smoothFilter.setSmooth(20);

		execute_detectBreaklines.setSmoothFilter(smoothFilter);

		return execute_detectBreaklines;
	}

	private static CreateGridParametersType buildExecuteCreateGrid() {
		final CreateGridParametersType execute_createGrid = new CreateGridParametersType();

		final DemPoints demPoints = new DemPoints();
		demPoints.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
				Gaja3dQNames.RP_DEM_POINTS));

		try {
			final URI demPointsHref = new URI(Client.class.getResource(
					DEMPOINTS_FILENAME).toURI().toString());
			demPoints.setHref(demPointsHref);
		} catch (final MalformedURIException e) {
			e.printStackTrace();
		} catch (final URISyntaxException e) {
			e.printStackTrace();
		}
		execute_createGrid.setDemPoints(demPoints);

		final GridX gridX = new GridX();
		gridX.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
				Gaja3dQNames.RP_GRID_X));
		gridX.setDx(20);
		execute_createGrid.setGridX(gridX);

		final GridY gridY = new GridY();
		gridY.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
				Gaja3dQNames.RP_GRID_Y));
		gridY.setDy(20);
		execute_createGrid.setGridY(gridY);

		return execute_createGrid;
	}

	private static SetResourceProperties_Element buildSetBoundaryResourceProperty() {
		final SetResourceProperties_Element setResourcePropertiesRequest = new SetResourceProperties_Element();
		final UpdateType update = new UpdateType();

		final Boundary boundary = new Boundary();
		boundary.setIdentifier(CodeTypeUtil.fillCodeType(new Identifier(),
				Gaja3dQNames.RP_BOUNDARY));

		try {
			final URI boundaryHref = new URI(Client.class.getResource(
					BOUNDARY_FILENAME).toURI().toString());
			boundary.setHref(boundaryHref);
		} catch (final MalformedURIException e) {
			e.printStackTrace();
		} catch (final URISyntaxException e) {
			e.printStackTrace();
		}
		update.set_any(new MessageElement[] { new MessageElement(
				Gaja3dQNames.RP_BOUNDARY, boundary) });
		setResourcePropertiesRequest.setUpdate(update);
		return setResourcePropertiesRequest;
	}

	// private static void callDescribeProcess(final Gaja3DPortType gaja3d,
	// final Capabilities capabilities) throws RemoteException {
	// final DescribeProcess describeProcess =
	// buildDescribeProcess(capabilities);
	// final ProcessDescriptions processDescriptions = gaja3d
	// .describeProcess(describeProcess);
	// System.out.println("Got process descriptions: "
	// + processDescriptions.getProcessDescription().length);
	// }
	//
	// private static Capabilities callGetCapabilities(final Gaja3DPortType
	// gaja3d)
	// throws RemoteException {
	// final GetCapabilitiesType getCapabilities = buildGetCapabilities();
	// final Capabilities capabilities = gaja3d
	// .getCapabilities(getCapabilities);
	// return capabilities;
	// }

	private static void setSecurity(final Gaja3DPortType gaja3d) {
		// final String secDescFile = Client.class.getResource(
		// "client_security_descriptor.xml").getFile();
		// ((Stub) gaja3d)._setProperty(Constants.CLIENT_DESCRIPTOR_FILE,
		// secDescFile);
		// ((Stub) gaja3d)._setProperty(GSIConstants.GSI_MODE,
		// GSIConstants.GSI_MODE_FULL_DELEG);
	}

	// private static Execute buildExecute() {
	// final Execute execute = new Execute();
	// execute.setService("WPS");
	// execute.setVersion("0.4");
	// final CodeType codeType = new CodeType("Gaja3d_createGrid");
	// execute.setIdentifier(codeType);
	// final DataInputsType dataInputs = new DataInputsType();
	// final IOValueType input = new IOValueType();
	// input.setTitle("The input");
	// input
	// .setIdentifier(new CodeType(
	// CreateGridSimulation.INPUT_DEM_POINTS));
	// try {
	// final ComplexValueReferenceType complexValueReference = new
	// ComplexValueReferenceType(
	// new URI("uri:TheTinLocation"), null, null, null);
	// input.setComplexValueReference(complexValueReference);
	// } catch (final MalformedURIException e) {
	// e.printStackTrace();
	// }
	// dataInputs.setInput(new IOValueType[] { input });
	// execute.setDataInputs(dataInputs);
	// final OutputDefinitionType output = new OutputDefinitionType();
	// output.setTitle("The output");
	// output
	// .setIdentifier(new CodeType(
	// CreateGridSimulation.OUTPUT_DEM_GRID));
	// final OutputDefinitionsType outputDefinitions = new
	// OutputDefinitionsType();
	// OutputDefinitionType[] outputs = new OutputDefinitionType[] { output };
	// outputDefinitions.setOutput(outputs);
	// execute.setOutputDefinitions(outputDefinitions);
	// return execute;
	// }
	//
	// private static DescribeProcess buildDescribeProcess(
	// Capabilities capabilities) {
	// final ProcessOfferings processOfferings = capabilities
	// .getProcessOfferings();
	// final ProcessBriefType[] processes = processOfferings.getProcess();
	// final CodeType[] processIds = new CodeType[processes.length];
	// int i = 0;
	// for (final ProcessBriefType processBriefType : processes) {
	// processIds[i++] = processBriefType.getIdentifier();
	// }
	// final DescribeProcess describeProcess = new DescribeProcess();
	// describeProcess.setService("WPS");
	// describeProcess.setVersion("0.4");
	// describeProcess.setIdentifier(processIds);
	// return describeProcess;
	// }
	//
	// private static GetCapabilitiesType buildGetCapabilities() {
	// final GetCapabilitiesType getCapabilities = new GetCapabilitiesType();
	// final AcceptVersionsType acceptVersions = new AcceptVersionsType();
	// acceptVersions.setVersion(new String[] { "0.4" });
	// getCapabilities.setAcceptVersions(acceptVersions);
	// return getCapabilities;
	// }

	private static void writeEPRtoFile(final EndpointReferenceType instanceEPR) {
		try {
			final String endpointString = ObjectSerializer.toString(
					instanceEPR, Gaja3dQNames.RESOURCE_REFERENCE);
			final FileWriter fileWriter = new FileWriter(EPR_FILENAME);
			final BufferedWriter bfWriter = new BufferedWriter(fileWriter);
			bfWriter.write(endpointString);
			bfWriter.close();
			System.out.println("Endpoint reference written to file "
					+ EPR_FILENAME);
		} catch (final SerializationException e) {
			e.printStackTrace();
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

	/*
	 * This method prints out MathService's resource properties by using the
	 * GetResourceProperty operation.
	 */
	private static void printResourceProperties(final Gaja3DPortType math)
			throws RemoteException {
		final GetMultipleResourceProperties_Element getMultipleResourcePropertiesRequest = new GetMultipleResourceProperties_Element();
		getMultipleResourcePropertiesRequest
				.setResourceProperty(Gaja3dQNames.ALL_RPS);
		final GetMultipleResourcePropertiesResponse response = math
				.getMultipleResourceProperties(getMultipleResourcePropertiesRequest);

		try {
			final MessageElement[] get_any = response.get_any();
			for (final MessageElement messageElement : get_any) {
				final String value = messageElement.getAsString();
				System.out.println(String.format("Value of RP %s: %s",
						messageElement.getQName(), value));
			}
		} catch (final Exception e) {
			e.printStackTrace();
		}

	}

}