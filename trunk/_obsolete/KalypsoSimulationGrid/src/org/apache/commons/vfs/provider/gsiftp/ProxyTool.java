/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.apache.commons.vfs.provider.gsiftp;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.GeneralSecurityException;
import java.security.PrivateKey;
import java.security.cert.X509Certificate;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;

import org.globus.common.CoGProperties;
import org.globus.gsi.CertUtil;
import org.globus.gsi.GSIConstants;
import org.globus.gsi.GlobusCredential;
import org.globus.gsi.GlobusCredentialException;
import org.globus.gsi.OpenSSLKey;
import org.globus.gsi.X509ExtensionSet;
import org.globus.gsi.bc.BouncyCastleCertProcessingFactory;
import org.globus.gsi.bc.BouncyCastleOpenSSLKey;
import org.globus.util.Util;

/**
 * Utility class to create a grid proxy
 * 
 * @author Vladimir Silva
 * 
 */
public class ProxyTool {
	// private Log log = LogFactory.getLog(ProxyTool.class);

	private X509Certificate[] certificates;

	private PrivateKey userKey = null;

	private int bits = 1024;

	// Valid for 12 hrs
	private int lifetime = 3600 * 12;

	private int proxyType = GSIConstants.GSI_2_PROXY;

	private boolean quiet = false;

	private GlobusCredential proxy = null;

	/*
	 * Verify proxy
	 */
	private void verify() throws GeneralSecurityException {
		RSAPublicKey pkey = (RSAPublicKey) this.certificates[0].getPublicKey();
		RSAPrivateKey prkey = (RSAPrivateKey) userKey;

		if (!pkey.getModulus().equals(prkey.getModulus())) {
			throw new GeneralSecurityException(
					"Certificate and private key specified do not match");
		}

	}

	private X509Certificate getCertificate() {
		return this.certificates[0];
	}

	/*
	 * Load a cert from a given path
	 */
	private void loadCertificates(String path) throws GeneralSecurityException,
			IOException {
		// certificates = CertUtil.loadCertificates(arg);
		certificates = new X509Certificate[] { CertUtil.loadCertificate(path) };
	}

	/**
	 * Load user key & decrypt it
	 * 
	 * @param keyPath
	 *            pth to the key
	 * @param pwd
	 *            decryption passphrase
	 * @throws GeneralSecurityException
	 */
	private void loadKey(String keyPath, String pwd)
			throws GeneralSecurityException {
		try {
			OpenSSLKey key = new BouncyCastleOpenSSLKey(keyPath);

			if (key.isEncrypted()) {
				key.decrypt(pwd);
			}

			userKey = key.getPrivateKey();

		} catch (IOException e) {
			throw new GeneralSecurityException("Error: Failed to load key: "
					+ keyPath);
		} catch (GeneralSecurityException e) {
			throw new GeneralSecurityException("Error: Wrong pass phrase");
		}
	}

	/*
	 * Create the actual proxy
	 */
	private void sign() {
		try {
			BouncyCastleCertProcessingFactory factory = BouncyCastleCertProcessingFactory
					.getDefault();

			X509ExtensionSet extSet = null;

			// if (proxyCertInfo != null) {
			// extSet = new X509ExtensionSet();
			// if (CertUtil.isGsi4Proxy(proxyType)) {
			// // RFC compliant OID
			// extSet.add(new ProxyCertInfoExtension(proxyCertInfo));
			// } else {
			// // old OID
			// extSet.add(new GlobusProxyCertInfoExtension(proxyCertInfo));
			// }
			// }

			proxy = factory.createCredential(certificates, userKey, bits,
					lifetime, proxyType, extSet);
		} catch (GeneralSecurityException e) {
			System.err.println("Failed to create a proxy: " + e.getMessage());
		}
	}

	/**
	 * Create a grid proxy
	 * 
	 * @param cert
	 *            Certificate path
	 * @param key
	 *            Key path
	 * @param pwd
	 *            Cert passphrase
	 * @param verify
	 *            proxy verification?
	 * @param globusStyle
	 * @param proxyFile
	 *            Path to the output proxy certificate
	 * @throws GeneralSecurityException
	 * @throws IOException
	 */
	public void createProxy(String cert, String key, String pwd,
			boolean verify, boolean globusStyle, String proxyFile)
			throws GeneralSecurityException, IOException {
		// log.debug("Certificate:" + cert);
		// log.debug("Private Key:" + key);

		loadCertificates(cert);

		// if (!quiet) {
		String dn = null;
		if (globusStyle) {
			dn = CertUtil.toGlobusID(getCertificate().getSubjectDN());
		} else {
			dn = getCertificate().getSubjectDN().getName();
		}
		// log.debug("Your identity: " + dn);
		// log.debug("Issuer: " + getCertificate().getIssuerDN());
		// }

		loadKey(key, pwd);

		// if (debug) {
		// log.debug("Using " + bits + " bits for private key");
		// }

		sign();

		if (verify) {
			verify();
		}

		// if (debug) {
		// log.debug("Saving proxy to: " + proxyFile);
		// }

		if (!quiet) {
			// log.debug("Your proxy is valid until "
			// + proxy.getCertificateChain()[0].getNotAfter());
		}

		OutputStream out = null;
		try {
			out = new FileOutputStream(proxyFile);

			// set read only permissions
			if (!Util.setFilePermissions(proxyFile, 600)) {
				System.err
						.println("Warning: Please check file permissions for your proxy file.");
			}

			// write the contents
			proxy.save(out);
		} catch (IOException e) {
			throw new IOException("Failed to save proxy to a file: "
					+ e.getMessage());
		} finally {
			if (out != null) {
				try {
					out.close();
				} catch (Exception e) {
				}
			}
		}

	}

	/**
	 * Create a grid proxy w/ default credentials
	 * 
	 * @param passPhrase
	 * @throws GeneralSecurityException
	 * @throws IOException
	 */
	public void createProxy(String passPhrase) throws GeneralSecurityException,
			IOException {
		if (!proxyExpired())
			return;

		CoGProperties properties = CoGProperties.getDefault();
		createProxy(properties.getUserCertFile(), properties.getUserKeyFile(),
				passPhrase, true, true, properties.getProxyFile());

	}

	/**
	 * Check for expired proxy
	 * 
	 * @return true if expired proxy
	 * @throws GlobusCredentialException
	 *             if no proxy found
	 */
	public boolean proxyExpired() {
		try {
			long t = new GlobusCredential(CoGProperties.getDefault()
					.getProxyFile()).getTimeLeft();
			return (t == 0) ? true : false;
		} catch (GlobusCredentialException e) {
			return true;
		}
	}

	/**
	 * @param args
	 */
	// public static void main(String[] args) {
	// // TODO Auto-generated method stub
	// try {
	// ProxyTool tool = new ProxyTool();
	// tool.createProxy("2p2dkdt");
	//			
	// } catch (Exception e) {
	// e.printStackTrace();
	// }
	// }
}
