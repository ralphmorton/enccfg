import * as crypto from 'crypto'

const ALGO = 'aes-256-cbc'
const ENCODING = 'base64'

export const genRand_ = (n) => () => {
  const bytes = crypto.randomBytes(n)
  const encoded = Buffer.from(bytes).toString(ENCODING)

  return encoded
}

export const encrypt_ = (nonceEncoded) => (keyEncoded) => (value) => () => {
  const nonce = Buffer.from(nonceEncoded, ENCODING)
  const key = Buffer.from(keyEncoded, ENCODING)

  const cipher = crypto.createCipheriv(ALGO, key, nonce)

  let ciphertext = cipher.update(value, 'utf-8', ENCODING)
  ciphertext += cipher.final(ENCODING)

  return ciphertext
}

export const decrypt_ = (nonceEncoded) => (keyEncoded) => (value) => () => {
  const nonce = Buffer.from(nonceEncoded, ENCODING)
  const key = Buffer.from(keyEncoded, ENCODING)

  const decipher = crypto.createDecipheriv(ALGO, key, nonce)

  let plaintext = decipher.update(value, ENCODING, 'utf-8')
  plaintext += decipher.final('utf-8')

  return plaintext
}
