package org.deegree_impl.model.cv;

import org.deegree.model.feature.Feature;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * Class for reading and writing RectifiedGridCoverages
 * 
 * @author N. Peiler
 *  
 */
public class RectifiedGridCoverageFactory {

	private static String NSRGC = "http://elbe.wb.tu-harburg.de/rectifiedGridCoverage";

	/**
	 * creates a RectifiedGridCoverage of a RectifiedGridCoverage-Feature
	 * 
	 * @param feat
	 *            RectifiedGridCoverage-Feature
	 * @return RectifiedGridCoverage
	 */
	public static RectifiedGridCoverage createRectifiedGridCoverage(Feature feat) {
		Feature feat_rectifiedGridCoverage = (Feature) feat
				.getProperty("RectifiedGridCoverageMember");
		RectifiedGridDomain rgDomain = (RectifiedGridDomain) feat_rectifiedGridCoverage
				.getProperty("rectifiedGridDomain");
		RangeSet rangeSet = (RangeSet) feat_rectifiedGridCoverage
				.getProperty("rangeSet");
		RectifiedGridCoverage gridCoverage = new RectifiedGridCoverage(
				rgDomain, rangeSet);
		return gridCoverage;
	}

	/**
	 * sets the data of a RectifiedGridCoverage in an gml-element
	 * 
	 * @param gridCoverage
	 * @param element
	 * @throws Exception
	 */
	public static void writeRectifiedGridCoverage(
			RectifiedGridCoverage gridCoverage, Element element)
			throws Exception {
		Document doc = element.getOwnerDocument();
		Element e_rectifiedGridCoverageMember = doc
				.createElement("RectifiedGridCoverageMember");
		Element e_rectifiedGridCoverage = doc
				.createElement("RectifiedGridCoverage");

		Element e_rectifiedGridDomain = doc
				.createElement("rectifiedGridDomain");
		RectifiedGridDomain gridDomain = gridCoverage.getGridDomain();
		ITypeHandler typeHandler = TypeRegistrySingleton.getTypeRegistry()
				.getTypeHandlerForTypeName(
						NSRGC + ":" + "RectifiedGridDomainType");
		typeHandler.marshall(gridDomain, e_rectifiedGridDomain);
		e_rectifiedGridCoverage.appendChild(e_rectifiedGridDomain);

		Element e_rangeSet = doc.createElement("rangeSet");
		RangeSet rangeSet = gridCoverage.getRangeSet();
		typeHandler = TypeRegistrySingleton.getTypeRegistry()
				.getTypeHandlerForTypeName(NSRGC + ":" + "RangeSetType");
		typeHandler.marshall(rangeSet, e_rangeSet);
		e_rectifiedGridCoverage.appendChild(e_rangeSet);

		e_rectifiedGridCoverageMember.appendChild(e_rectifiedGridCoverage);
		element.appendChild(e_rectifiedGridCoverageMember);
	}
}