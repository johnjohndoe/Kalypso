package org.kalypso.gaja3d.service.internal;

import javax.xml.namespace.QName;

import org.apache.axis.types.URI;
import org.apache.axis.types.URI.MalformedURIException;
import org.kalypso.gaja3d.service.stubs.Identifier;

public class CodeTypeUtil {

	public static Identifier fillCodeType(final Identifier codeType,
			final QName qName) {
		try {
			codeType.setCodeSpace(new URI(qName.getNamespaceURI()));
		} catch (final MalformedURIException e) {
			e.printStackTrace();
		}
		codeType.set_value(qName.getLocalPart());
		return codeType;
	}

}
