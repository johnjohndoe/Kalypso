package org.kalypso.gaja3d.service.internal;

import javax.xml.namespace.QName;

import net.opengeospatial.ows.stubs.CodeType;

import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.kalypso.gaja3d.service.stubs.Identifier;

public class CodeTypeUtil {

	public static Identifier fillCodeType(final Identifier identifier,
			final QName qName) {
		final CodeType codeType = new CodeType();
		codeType.set_value(qName.getLocalPart());
		try {
			codeType.setCodeSpace(new URI(qName.getNamespaceURI()));
		} catch (final MalformedURIException e) {
			e.printStackTrace();
		}
		identifier.set_value(codeType);
		return identifier;
	}

}
